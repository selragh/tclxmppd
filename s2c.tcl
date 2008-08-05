# s2c.tcl - Copyright (C) 2006 Pat Thoyts <patthoyts@users.sourceforge.net>
#
#  A Tcl implementation of the Jabber server-to-client protocol.
#  See http://www.jabber.org/
#
# RFC 3920 [http://www.ietf.org/rfc/rfc3921.txt]
# RFC 3921 [http://www.ietf.org/rfc/rfc3921.txt]
#
# -------------------------------------------------------------------------
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.
# -------------------------------------------------------------------------

package require xmppd::core;            # tclxmppd 
package require uuid;                   # tcllib
package require sha1;                   # tcllib
package require base64;                 # tcllib
package require SASL;                   # tcllib 1.8
package require dns 1.2.1;              # tcllib 1.8
# optional
package require tls;                    # tls
catch {package require Iocpsock};       # win32 ipv6 support

 
namespace eval ::xmppd {}
namespace eval ::xmppd::s2c {

    variable version 1.0.0
    variable rcsid {$Id: s2c.tcl,v 1.5 2006/04/17 10:14:47 pat Exp $}

    namespace export start stop

    variable options
    if {![info exists options]} {
        array set options {
            s2c:address      {0.0.0.0 5222 :: 5222}
            s2c:handler      {}
            s2c:authenticate {}
        }
    }

    variable uid
    if {![info exists uid]} {
        set uid 0
    }

    namespace import -force ::xmppd::configure ::xmppd::cget \
        ::xmppd::Pop ::xmppd::xmlns ::xmppd::jid
}

# -------------------------------------------------------------------------

proc ::xmppd::s2c::start {} {
    variable listeners
    if {![info exists listeners]} {set listeners {}}
    set scmd ::socket
    if {[llength [info commands ::socket2]] > 0} { set scmd ::socket2 }
    foreach {addr port} [cget -s2c:address] {
        if {[ip::is ipv6 $addr] && [package provide Iocpsock] == {}} {
            continue
        }
        set srv [$scmd -server [namespace current]::Accept -myaddr $addr $port]
        lappend listeners $srv
        Log notice "XMPP s2c listening on $addr:$port ($srv)"
    }
    return
}

proc ::xmppd::s2c::stop {} {
    variable listeners
    foreach Channel [info vars [namespace current]::channel*] {
        Close $Channel
    }
    foreach srv $listeners {
        catch {
            set info [fconfigure $srv -sockname]
            close $srv
            Log notice "XMPP s2c stopped listening on [lindex $info 0]:[lindex $info 2]"
        } msg
        puts stderr $msg
    }
    set listeners {}
    return
}

# -------------------------------------------------------------------------

proc ::xmppd::s2c::_configure {args} {
    variable options
    if {[llength $args] < 1} {
        set r {}
        foreach opt [lsort [array names options]] {
            lappend r -$opt $options($opt)
        }
        return $r
    }

    set cget [expr {[llength $args] == 1 ? 1 : 0}]
    while {[string match -* [set option [lindex $args 0]]]} {
        switch -glob -- $option {
            -s2c:address {
                if {$cget} {
                    return $options(s2c:address)
                } else {
                    set options(s2c:address) [Pop args 1]
                }
            }
            -s2c:handler {
                if {$cget} {
                    return $options(s2c:handler)
                } else {
                    set options(s2c:handler) [Pop args 1]
                }
            }
            -s2c:authenticate {
                if {$cget} {
                    return $options(s2c:authenticate)
                } else {
                    set options(s2c:authenticate) [Pop args 1]
                }
            }
            -- { Pop args ; break }
            default {
                return -code error "bad option \"$option\""
            }
        }
        Pop args
    }
    return
}

proc ::xmppd::s2c::route {from to xml} {
    # find the right channel and put the xml on it.
    # if there is no channel then it's probably time to support
    # stored messages.
    set Channel [FindChannel $to]
    if {$Channel ne {}} {
        WriteTo $Channel $xml
    } else {
        # FIX ME: create an error and route it to the from jid.
        Log warn "FIX handling stanzas to disconnected clients"
    }
}

# xmppd::s2c::Accept --
#
#	The Accept procedure is run in response to a new client connection.
#	We create a Channel array to hold all information required to 
#	maintain the communications with this client.
#
proc ::xmppd::s2c::Accept {chan clientaddr clientport} {
    variable options
    Log notice "XMPP s2c accept connect from $clientaddr:$clientport on $chan"
    set Channel [CreateChannel]
    upvar #0 $Channel channel
    set channel(address) $clientaddr
    set channel(port)    $clientport
    set channel(sock)    $chan
    set channel(state)   connected
    set channel(parser) \
        [wrapper::new \
             [list [namespace current]::OnOpenStream $Channel] \
             [list [namespace current]::OnCloseStream $Channel] \
             [list [namespace current]::OnInput $Channel] \
             [list [namespace current]::OnError $Channel]]
    #-namespace 0

    fconfigure $chan -translation binary -encoding utf-8 \
        -buffering none -blocking 0
    fileevent $chan readable [list [namespace current]::Read $Channel]
}

# xmppd::s2c::CreateChannel --
#
#	Create a new channel to manage information about a client connection
#	Any per-connection status will be kept here (eg: locale)
#
proc ::xmppd::s2c::CreateChannel {} {
    variable uid
    set Channel [namespace current]::channel[incr uid]
    array set $Channel {
        sock {} address {} port {} jid {} parser {}
        resource {} state unconnected lang en
    }
    return $Channel
}

# Find a channel by target jid
proc ::xmppd::s2c::FindChannel {jid} {
    set r {}
    set jid [jid !resource $jid]
    foreach Channel [info vars [namespace current]::channel*] {
        upvar #0 $Channel channel
        if {$channel(jid) eq $jid} {
            lappend r $Channel
        }
    }
    return $r
}

# xmppd::s2c::Write --
#
#	Called when the client channnel becomes writable for the first time.
#	We begin basic XMPP comms initialization from the server side.
#	FIX ME: in s2c this could be done in the first OpenStream handler.
#
proc ::xmppd::s2c::Write {Channel} {
    upvar #0 $Channel channel
    fileevent $channel(sock) writable {}
    set xml "<?xml version='1.0' encoding='utf-8'?>"
    append xml "<stream:stream xmlns='jabber:client'"
    append xml " xmlns:stream='http://etherx.jabber.org/streams'"
    append xml " version='1.0'>"
    WriteTo $Channel $xml
}

# xmppd::s2c::Read --
#
#	Any data available on the client channel is read here and passed to
#	the XML parser which will then call to the registered handler 
#	procedures.
#
proc ::xmppd::s2c::Read {Channel} {
    upvar #0 $Channel channel
    if {[eof $channel(sock)]} {
        fileevent $channel(sock) readable {}
        Log warn "- EOF on $Channel ($channel(sock))"
        OnCloseStream $Channel
    }
    set xml [read $channel(sock)]
    if {[string length [string trim $xml]] > 0} {
        Log debug "< $Channel $xml"
        wrapper::parse $channel(parser) $xml
    }
}

# xmppd::s2c::WriteTo --
#
#	Send a chunk of data to the client (with logging).
#
proc ::xmppd::s2c::WriteTo {Channel data} {
    upvar #0 $Channel channel
    Log debug "> $Channel $data"
    puts -nonewline $channel(sock) $data
}

# Raise --
#
#	Raise a stream error and close the route.
#
proc ::xmppd::s2c::Raise {Channel type {text ""}} {
    upvar #0 $Channel channel
    set xml "<stream:error>"
    append xml "<$type xmlns='[xmlns streams]'/>"
    if {$text ne ""} {
        append xml "<text xml:lang='$channel(lang)'\
            xmlns='[xmlns streams]'>[xmlquote $text]</text>"
    }
    append xml "</stream:error>"
    WriteTo $Channel $xml
    Close $Channel
}

# xmppd::s2c::Log
#
#
#
proc ::xmppd::s2c::Log {level msg} {
    ::xmppd::Log s2c $level $msg 
}

# Error --
#
#	Generate the XML body for an error stanza. See section 9.3.2
#
proc ::xmppd::s2c::Error {Channel error type {text ""}} {
    upvar #0 $Channel channel
    set xml "<error type='$type'>"
    append xml "<$error xmlns='[xmlns stanzas]'/>"
    if {$text ne ""} {
        append xml "<text xml:lang='$channel(lang)' xmlns='xmlns stanzas]'>[xmlquote $text]</text>"
    }
    append xml "</error>"
    return $xml
}

# Close --
#
#	Shut down a route. We close the channel and clear up our state.
#
#	FIX ME: we need to clean up the parser state too -- we currently
#	leak the parsers resources.
#
proc ::xmppd::s2c::Close {Channel} {
    WriteTo $Channel "</stream:stream>"
    OnCloseStream $Channel
}

proc xmppd::s2c::xmlquote {s} {
    variable xmlmap
    if {![info exists xmlmap]} {
        set xmlmap {"&" "&amp;" "<" "&lt;" ">" "&gt;" "\"" "&quot;" "'" "&apos;"}
        for {set n 0} {$n < 32} {incr n} {
            lappend xmlmap [format %c $n] [format "&#%02x;" $n]
        }
    }
    string map $xmlmap $s
}

# -------------------------------------------------------------------------

proc ::xmppd::s2c::OnOpenStream {Channel args} {
    variable options
    upvar #0 $Channel channel

    # RFC3920 4.4.1: no version means assume 0.0
    array set attr {version 0.0}
    array set attr $args
    Log debug "OPENSTREAM $channel(sock) [array get attr]"

    set channel(id) [string map {- {}} [uuid::uuid generate]]
    
    set xml "<?xml version='1.0' encoding='utf-8'?>"
    append xml "<stream:stream xmlns='[xmlns client]'\
            xmlns:stream='[xmlns stream]'\
            id='$channel(id)' from='[cget -domain]' version='1.0'>"

    # RFC3920 4.6: Stream Features
    if {$attr(version) >= 1.0} {
        append xml "<stream:features>"
        # Check for previous SASL negotiation
        if {$channel(state) eq "authorized"} {
            # RFC3920 7: Resource binding
            append xml "<bind xmlns='[xmlns bind]'/>"
            
            # Include any registered xmppd features
            # This may need extending if there are more complex features to do.
            foreach {name uri} [cget -features] {
                append xml "<$name xmlns='$uri'/>"
            }
        } else {
            if {[package provide tls] ne {} \
                    && $channel(state) eq "connected" \
                    && [file exists [cget -certfile]] \
                    && [file exists [cget -keyfile]]} {
                append xml "<starttls xmlns='[xmlns tls]'/>"
            }
            # RFC3920 6.1: Use of SASL
            append xml "<mechanisms xmlns='[xmlns sasl]'>"
            append xml "<mechanism>DIGEST-MD5</mechanism>"
            append xml "<mechanism>PLAIN</mechanism>"
            append xml "</mechanisms>"
        }
        append xml "</stream:features>"
    }
    WriteTo $Channel $xml
}

proc ::xmppd::s2c::OnCloseStream {Channel} {
    upvar #0 $Channel channel

    #foreach Session [FindSession channel $Channel] {
    #    Log debug "closed session $Session"
    #    unset $Session
    #}

    catch {close $channel(sock)}
    wrapper::reset $channel(parser)
    catch {unset channel} msg
    Log notice "- $Channel closed: $msg"
}

proc ::xmppd::s2c::OnError {Channel code args} {
    Log error "- $Channel error $code"
    WriteTo $Channel "</stream:stream>"
    OnCloseStream $Channel
}

# For xmpp service: authzid (login) is the jid authid (username) 
# is the jid node.
proc ::xmppd::s2c::SASLCallback {Channel context command args} {
    variable options
    upvar #0 $Channel channel
    switch -exact -- $command {
        password {
            #Log debug "SASL retrieve password for authid [lindex $args 0] '$args'"
            set channel(jid) [lindex $args 0]@[cget -domain]
            return [eval [linsert $args 0 [cget -s2c:authenticate]]]
        }
        realm { return [cget -domain] }
        default {
            return -code error "SASL callback $command used. Implement it"
        }
    }
}

proc ::xmppd::s2c::SASLSuccess {Channel} {
    upvar #0 $Channel channel
    SASL::cleanup $channel(sasl)
    set channel(state) authorized
    WriteTo $Channel "<success xmlns='[xmlns sasl]'/>"
    wrapper::reset $channel(parser)
}

proc ::xmppd::s2c::SASLFailure {Channel msg} {
    set xml "<failure xmlns='[xmlns sasl]'><temporary-auth-failure/>"
    if {$msg ne ""} {
        append xml "<text>[xmlquote $msg]</text>"
    }
    append xml "</failure>"
    WriteTo $Channel $xml
    Close $Channel
}

proc ::xmppd::s2c::OnInput {Channel xmllist} {
    variable options
    upvar #0 $Channel channel

    foreach {cmd attr close value children} $xmllist break
    array set a {xmlns {} from {} to {}}
    array set a $attr

    switch -exact -- $cmd {
        starttls {
            Log debug "- starttls $xmllist"
            if {[package provide tls] eq {}} {
                set xml "<failure xmlns='[xmlns tls]'><temporary-auth-failure/></failure>"
                WriteTo $Channel $xml
                Close $Channel
            } else {
                set xml "<proceed xmlns='[xmlns tls]'/>"
                set channel(state) tls
                WriteTo $Channel $xml
                flush $channel(sock)
                wrapper::reset $channel(parser)
                tls::import $channel(sock) -server 1 -tls1 1 -ssl3 1 -ssl2 0 \
                    -keyfile [cget -keyfile] -certfile [cget -certfile]
            }
        }

        auth {
            Log debug "- auth $xmllist"
            if {$a(xmlns) eq [xmlns sasl]} {
                set channel(sasl) \
                    [SASL::new -service xmpp -type server \
                         -mechanism $a(mechanism) \
                         -callback [list [namespace origin SASLCallback] $Channel]]
                if {[catch {set more [SASL::step $channel(sasl) [base64::decode $value]]} err]} {
                    SASLFailure $Channel $err
                } else {
                    if {$more} {
                        set xml "<challenge xmlns='[xmlns sasl]'>[base64::encode -maxlen 0 [SASL::response $channel(sasl)]]</challenge>"
                        WriteTo $Channel $xml
                    } else {
                        SASLSuccess $Channel
                    }
                }
            } else {
                # FIX ME
                Raise $Channel fix-me-error
            }
        }
        response {
            Log debug "- response $xmllist"
            if {[info exists channel(sasl)] && $channel(sasl) ne ""} {
                if {[catch {set more [SASL::step $channel(sasl) [base64::decode $value]]} err]} {
                    SASLFailure $Channel $err
                } else {
                    if {$more} {
                        set xml "<challenge xmlns='[xmlns sasl]'>[base64::encode -maxlen 0 [SASL::response $channel(sasl)]]</challenge>"
                        WriteTo $Channel $xml
                    } else {
                        SASLSuccess $Channel
                    }
                }
            } else {
                Raise $Channel unsupported-stanza-type                 
            }
        }
        
        abort {
            Log debug "- abort $xmllist"
            if {[info exists channel(sasl)] && $channel(sasl) ne ""} {
                unset channel(sasl)
                set xml "<failure xmlns='[xmlns sasl]'><aborted/></failure>"
                WriteTo $Channel $xml
                Close $Channel
            } else {
                Raise $Channel unsupported-stanza-type                 
            }
        }
        
        
        iq {
            Log debug "- iq $xmllist { $channel(state) }"
            if {$channel(state) eq "authorized"} {
                set bind [lindex [wrapper::getchildswithtagandxmlns \
                                      $xmllist bind [xmlns bind]] 0]
                Log debug "[string repeat - 60]\n$bind\n[string repeat - 60]\n"
                if {$bind ne {}} {
                    set channel(state) bound
                    set rsrc [lindex [wrapper::getchildswithtag $bind resource] 0]
                    set channel(resource) [wrapper::getcdata $rsrc]
                    Log debug "[string repeat - 60]\n$channel(resource):$rsrc\n[string repeat - 60]\n"
                    if {$channel(resource) eq ""} {
                        set channel(resource) [base64::encode -maxlen 0 [uuid::generate]]
                    }
                    set jid $channel(jid)/$channel(resource)
                    set xml "<iq type='result' id='$a(id)'><bind\
                        xmlns='[xmlns bind]'><jid>$jid</jid></bind></iq>"
                    WriteTo $Channel $xml
                    return
                } else {
                    Raise $Channel not-authorized
                    return
                }
            }
            Routing $Channel $xmllist
        }

        message -
        presence {
            Routing $Channel $xmllist
        }

        default {
            Log debug "- event $xmllist"
            Raise $Channel unsupported-stanza-type 
        }
    }
}

proc ::xmppd::s2c::Routing {Channel xmllist} {
    # Ensure we always have a from attribute (clients don't have to send one)
    if {[wrapper::getattribute $xmllist from] eq ""} {
        upvar #0 $Channel channel
        set attr [wrapper::getattrlist $xmllist]
        set jid $channel(jid)
        if {$channel(resource) ne ""} { append jid /$channel(resource) }
        set attr [wrapper::setattr $attr from $jid]
        set xmllist [wrapper::setattrlist $xmllist $attr]
    }
    
    Log debug "Routing: $xmllist"

    # stanzas addressed to this server need to be passed to the handler
    # as do stanzas with no 'to' jid. The rest are routed.
    set to   [wrapper::getattribute $xmllist to]
    set from [wrapper::getattribute $xmllist from]
    if {$to eq "" || $to eq [cget -domain]} {
        Log debug "Routing calling local handler"
        CallHandler $Channel $xmllist
    } else {
        Log debug "Routing route $from $to"
        xmppd::route $from $to [wrapper::createxml $xmllist]
    }
}

proc ::xmppd::s2c::CallHandler {Channel xmllist} {
    set tag [wrapper::gettag $xmllist]
    set handler [cget -s2c:handler]
    if {$handler ne ""} {
        if {[catch {$handler $xmllist} err]} {
            Log error "s2c:handler error: $err"
        }
    } else {
        Log error "No handler defined for \"$tag\" stanza"
        set t [list internal-server-error [list xmlns [xmlns stanzas]] 1]
        set e [list error {type cancel} 0 {} [list $t]]
        set r [list $tag {} 0 {} [list $e]]
        set a [list type error from [wrapper::getattribute $xmllist to] \
                   to [wrapper::getattribute $xmllist from]]
        if {[set id [wrapper::getattribute $xmllist id]] ne ""} {
            set a [wrapper::setattr $a id $id]
        }
        set r [wrapper::setattrlist $r $a]
        WriteTo $Channel [wrapper::createxml $r]
    }
}

# -------------------------------------------------------------------------

if {[llength [info commands ::xmppd::register]] > 0} {
    ::xmppd::register module xmppd::s2c
}

package provide xmppd::s2c $::xmppd::s2c::version

# -------------------------------------------------------------------------
# Local variables:
#   mode: tcl
#   indent-tabs-mode: nil
# End:

