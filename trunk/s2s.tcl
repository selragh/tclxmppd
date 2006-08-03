# s2s.tcl - Copyright (C) 2004 Pat Thoyts <patthoyts@users.sourceforge.net>
#
#  A Tcl implementation of the Jabber server-to-server protocol.
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
package require logger;                 # tcllib
package require dns 1.2.1;              # tcllib 1.8

namespace eval ::xmppd {}
namespace eval ::xmppd::s2s {

    variable version 1.0.0
    variable rcsid {$Id: s2s.tcl,v 1.15 2006/04/17 10:14:47 pat Exp $}

    namespace export start stop route

    variable options
    if {![info exists options]} {
        array set options {
            s2s:secret   secret
            s2s:address  {0.0.0.0 5269 :: 5269}
            s2s:handler  {}
        }
    }

    variable uid
    if {![info exists uid]} {
        set uid 0
    }

    # Select the first nameserver available (if any)
    foreach ns [dns::nameservers] {
        if {[ip::is ipv6 $ns]} { continue }
        dns::configure -nameserver $ns -protocol tcp
        break
    }

    namespace import -force ::xmppd::configure ::xmppd::cget \
        ::xmppd::Pop ::xmppd::xmlns ::xmppd::jid
}

# -------------------------------------------------------------------------

proc ::xmppd::s2s::start {} {
    variable listeners
    if {![info exists listeners]} {set listeners {}}
    set scmd ::socket
    if {[llength [info commands ::socket2]] > 0} { set scmd ::socket2 }
    foreach {addr port} [cget -s2s:address] {
        if {[ip::is ipv6 $addr] && [package provide Iocpsock] == {}} {
            continue
        }
        set srv [$scmd -server [namespace current]::Accept -myaddr $addr $port]
        lappend listeners $srv
        Log notice "XMPP s2s listening on $addr:$port"
    }
    return
}

proc ::xmppd::s2s::stop {} {
    variable listeners
    foreach Channel [info vars [namespace current]::channel*] {
        Close $Channel
    }
    foreach srv $listeners {
        catch {
            set info [fconfigure $srv -sockname]
            close $srv
            Log notice "XMPP s2s stopped listening on [lindex $info 0]:[lindex $info 2]"
        } msg
        puts stderr $msg
    }
    set listeners {}
    return
}

# -------------------------------------------------------------------------

proc ::xmppd::s2s::_configure {args} {
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
            -s2s:secret {
                if {$cget} {
                    return $options(s2s:secret)
                } else {
                    set options(s2s:secret) [Pop args 1]
                }
            }
            -s2s:address {
                if {$cget} {
                    return $options(s2s:address)
                } else {
                    set options(s2s:address) [Pop args 1]
                }
            }
            -s2s:handler {
                if {$cget} {
                    return $options(s2s:handler)
                } else {
                    set options(s2s:handler) [Pop args 1]
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

proc ::xmppd::s2s::route {args} {
    array set opts {-from {} -to {}}
    while {[string match -* [set option [lindex $args 0]]]} {
        switch -exact -- $option {
            -to -
            -from {
                set jid [jid domain [Pop args 1]]
                if {[string length $jid] > 0} {
                    puts "$option jid: '$jid'"
                    set opts($option) $jid
                }
            }
            -- { Pop args; break }
            default { break }
        }
        Pop args
    }
    
    foreach opt {-from -to} {
        if {[string length $opts($opt)] < 1} {
            return -code error "invalid argument \"$opt\":\
                valid jids are required for both -from and -to"
        }
    }

    if {[llength $args] != 1} {
        return -code error "wrong # args: must be\
             \"route -from jid -to jid xml\""
    }
    set data [lindex $args 0]
    if {[string length $data] < 1} {
        Log warn "[lindex [info level 0] 0] no data to send!"
        return
    }

    Queue $opts(-from) $opts(-to) $data
    return
}

# look up the IP address for the server of a given JID.
# This uses the DNS SRV records as described in RFC3920 and
# falls back to DNS A record resolution if no SRV records.
proc ::xmppd::s2s::resolve {jid} {
    set hostname [jid domain $jid]
    set result {}
    set port   5269
    foreach srvd {"_xmpp-server._tcp" "_jabber._tcp"} {
        set tok [dns::resolve "${srvd}.${hostname}" -type SRV]
        if {[dns::status $tok] eq "ok"} {
            set answers {}
            foreach rr [dns::result $tok] {
                array set res $rr
                if {[info exists res(type)] \
                        && $res(type) eq "SRV" \
                        && [llength $res(rdata)] > 0} {
                    lappend answers $res(rdata)
                }
            }
            lsort -index 1 $answers
            array set rrr [lindex $answers 0]
            set port $rrr(port)
            if {[ip::version $rrr(target)] == -1} {
                set hostname $rrr(target)
            } else {
                set result [list $rrr(target) $port]
            }
        }
        dns::cleanup $tok
        if {[llength $result] > 0} {break}
    }

    if {[llength $result] == 0} {
        set tok [dns::resolve $hostname -type A]
        if {[dns::status $tok] eq "ok"} {
            set result [list [dns::address $tok] $port]
        }
        dns::cleanup $tok
    }

    return $result
}

# -------------------------------------------------------------------------

# Holds info about a socket stream.
# The from and to items are temporary as routes are held on session objects.
# Once the session is created, we erase the from and to items.
proc ::xmppd::s2s::CreateChannel {} {
    variable uid
    set Channel [namespace current]::channel[incr uid]
    array set $Channel {sock {} address {} port {} from {} to {} parser {}}
    return $Channel
}

# Find a session for a given route
proc ::xmppd::s2s::FindChannel {dir addr} {
    foreach Channel [info vars [namespace current]::channel*] {
        upvar #0 $Channel channel
        if {$channel(dir) eq $dir && $channel(address) eq $addr} {
            return $Channel
        }
    }
    return {}
}

proc ::xmppd::s2s::ListChannels {} {
    set r {}
    foreach Channel [info vars [namespace current]::channel*] {
        upvar #0 $Channel channel
        lappend r [list [namespace tail $Channel] \
                       $channel(dir) $channel(address)]
    }
    return $r
}

proc ::xmppd::s2s::ListSessions {} {
    set r {}
    foreach Session [info vars [namespace current]::session*] {
        upvar #0 $Session session
        lappend r [list [namespace tail $Session] \
                       $session(from) $session(to) \
                       [namespace tail $session(channel)]]
    }
    return $r
}

proc ::xmppd::s2s::CreateSession {} {
    variable uid
    set Session [namespace current]::session[incr uid]
    array set $Session {
        chan {} from {} to {} id {} state new
        queue {} after {} key {} parser {}
    }
    return $Session
}

# Find a session for a given route
proc ::xmppd::s2s::FindSession {op args} {
    set r {}
    switch -exact -- $op {
        id {
            set id [lindex $args 0]
            foreach Session [info vars [namespace current]::session*] {
                upvar #0 $Session session
                if {[info exists session(id)] && $session(id) eq $id} {
                    lappend r $Session
                    break
                }
            }
        }
        name {
            foreach {from to} $args break
            foreach Session [info vars [namespace current]::session*] {
                upvar #0 $Session session
                if {[info exists session(from)] && $session(from) eq $from 
                    && [info exists session(to)] && $session(to) eq $to} {
                    lappend r $Session
                    Log debug " Found session $r: $from -> $to"
                    break
                }
            }
        }
        channel {
            set Channel [lindex $args 0]
            foreach Session [info vars [namespace current]::session*] {
                upvar #0 $Session session
                if {[info exists session(channel)] 
                    && $session(channel) eq $Channel} {
                    lappend r $Session
                }
            }
        }
        default {
            return -code error "invalid operation \"$op\":\
                must be one of \"id\", \"name\" or \"channel\""
        }
    }
    return $r
}

proc ::xmppd::s2s::Queue {from to data} {
    Log debug "Queue message -from $from -to $to"
    # Either find an open session or open a new one.
    set Session [FindSession name $from $to]
    if {[llength $Session] < 1} {
        set Channel [Open $from $to]
        set [set Channel](queue) $data
    } else {
        # Queue our message for transmission by this session.
        upvar #0 $Session session
        lappend session(queue) $data
        # schedule xmit if not already scheduled.
        if {[llength $session(queue)] == 1} {
            set session(after) \
                [after 10 [list [namespace current]::Flush $Session]]
        }
    }
    return
}

proc ::xmppd::s2s::Flush {Session} {
    upvar #0 $Session session
    if {![info exists session]} {return}
    if {[info exists session(channel)]} {
        upvar #0 $session(channel) channel
        catch {after cancel $session(after)}
        if {$session(state) eq "valid"} {
            set data [lindex $session(queue) 0]
            if {![catch {WriteTo $session(channel) $data} err]} {
                Pop session(queue)
            }
        }
    }
    if {[llength $session(queue)] != 0} {
        set session(after) \
            [after 1000 [list [namespace current]::Flush $Session]]
    }
    return
}

# Open
# Opens a new connection to a jabber server and creates our session state
#
# TODO: check for config details per remote site?
#       use DNS to look for the SRV resources.
proc ::xmppd::s2s::Open {from to} {

    # First, resolve the hostname. If possible we can re-use a connection that
    # already exists.
    
    if {[llength [set addr [resolve $to]]] < 1} {
        return -code error "hostname invalid: \"$to\" failed to resolve ip address"
    }
    
    set Channel [FindChannel out [lindex $addr 0]]
    if {[llength $Channel] < 1} {
        set Channel [CreateChannel]
        upvar #0 $Channel channel
        set channel(dir)     out
        set channel(address) [lindex $addr 0]
        set channel(port)    [lindex $addr 1]
        set channel(from)    $from
        set channel(to)      $to
        set channel(parser) \
            [wrapper::new \
                 [list [namespace current]::OnOpenStream $Channel] \
                 [list [namespace current]::OnCloseStream $Channel] \
                 [list [namespace current]::OnInput $Channel] \
                 [list [namespace current]::OnError $Channel] \
                 -namespace 0]

        set sock [socket -async $channel(address) $channel(port)]
        set channel(sock) $sock
        fconfigure $sock -buffering none -blocking 0 \
            -encoding utf-8 -translation lf
        fileevent $sock writable [list [namespace current]::Write $Channel]
        fileevent $sock readable [list [namespace current]::Read $Channel]
    }

    return $Channel
}

proc ::xmppd::s2s::Accept {chan clientaddr clientport} {
    variable options
    Log notice "XMPP s2s accept connect from $clientaddr:$clientport on $chan"
    # RFC3920 8.3(5): The remote server opens a stream back here based upon
    #                 the domain name we provided.
    set Channel [CreateChannel]
    upvar #0 $Channel channel
    set channel(dir)     in
    set channel(address) $clientaddr
    set channel(port)    $clientport
    set channel(sock)    $chan
    set channel(parser) \
        [wrapper::new \
             [list [namespace current]::OnOpenStream $Channel] \
             [list [namespace current]::OnCloseStream $Channel] \
             [list [namespace current]::OnInput $Channel] \
             [list [namespace current]::OnError $Channel] \
             -namespace 0]

    fconfigure $chan -translation binary -encoding utf-8 \
        -buffering none -blocking 0
    fileevent $chan readable [list [namespace current]::Read $Channel]
}

proc ::xmppd::s2s::Write {Channel} {
    upvar #0 $Channel channel
    fileevent $channel(sock) writable {}
    set xml "<?xml version='1.0' encoding='utf-8'?>"
    append xml "<stream:stream xmlns='[xmlns server]'"
    append xml " xmlns:stream='[xmlns stream]'"
    append xml " xmlns:db='[xmlns dialback]'"
    append xml " version='1.0'>"
    WriteTo $Channel $xml
}

proc ::xmppd::s2s::Read {Channel} {
    upvar #0 $Channel channel
    if {[eof $channel(sock)]} {
        fileevent $channel(sock) readable {}
        Log warn "- EOF on $Channel ($channel(sock))"
        OnCloseStream $Channel
    }
    set xml [read $channel(sock)]
    if {[string length [string trim $xml]] > 0} {
        Log debug "< $channel(sock) $xml"
        wrapper::parse $channel(parser) $xml
    }
}

proc ::xmppd::s2s::WriteTo {Channel data} {
    upvar #0 $Channel channel
    Log debug "> $channel(sock) $data"
    puts -nonewline $channel(sock) $data
}


# Raise --
#
#	Raise a stream error and close the route.
#
proc ::xmppd::s2s::Raise {Channel type args} {
    # FIX ME - close just the session!?
    set xml "<stream:error><$type xmlns='[xmlns streams]'/>"
    WriteTo $Channel $xml
    Close $Channel
}

# Close --
#
#	Shut down a route. We close the channel and clear up our state.
#
#	FIX ME: we need to clean up the parser state too -- we currently
#	leak the parsers resources.
#
proc ::xmppd::s2s::Close {Channel} {
    # FIX ME - this probably should just close a session.
    WriteTo $Channel "</stream:stream>"
    OnCloseStream $Channel
}

# xmppd::s2s::Log
#
#
#
proc ::xmppd::s2s::Log {level msg} {
    ::xmppd::Log s2s $level $msg 
}
# -------------------------------------------------------------------------

proc ::xmppd::s2s::OnOpenStream {Channel args} {
    variable options
    upvar #0 $Channel channel

    array set attr {version 0.0}
    array set attr $args
    Log debug "OPENSTREAM $channel(sock) [array get attr]"

    if {[info exists attr(id)]} {

        # RFC3920 8.3(3): Remote server sends up a unique session id.
        #                 The from and to elements are optional here.
        #                 We must reject invalid namespace.
        #if {![info exists attr(xmlns)] 
        #    || $attr(xmlns) ne "http://etherx.jabber.org/streams"} {
        #    return [Raise $Channel invalid-namespace]
        #}
        set Session [CreateSession]
        upvar #0 $Session session
        set session(channel) $Channel
        set session(from)    $channel(from)
        set session(to)      $channel(to)
        set session(id)      $attr(id)
        if {[info exists channel(queue)]} {
            set session(queue)   [list $channel(queue)]
        }
        set channel(from)    {};        # clean up temporary channel items
        set channel(to)      {};        # 
        set channel(queue)   {}
        

        # RFC3920 8.3(4): The Originating Server (us) sends a dialback key 
        #                 to the Receiving Server (them)
        #
        # JID-0185: Dialback key generation and validation
        #
        set key "$session(id):$session(to):$session(from):[cget -s2s:secret]"
        set session(key) [sha1::sha1 $key]

        set xml "<db:result xmlns:db='[xmlns dialback]'\
            to='$session(to)' from='$session(from)'>$session(key)</db:result>"
        set session(state) dialback
        WriteTo $Channel $xml

    } else {

        # RFC3920 8.3(6): The Receiving Server (them) sends the Authoritative
        #                 Server (us) a stream header. From and to are
        #                 optional. We MUST reject invalid namespaces.
        # implemented wrong - check that the stream namespace is correct.
        #if {![info exists attr(xmlns)] || $attr(xmlns) ne [xmlns stream]} {
        #    return [Raise $Channel invalid-namespace]
        #}

        # RFC3920 8.3(7): The Authoritative Server (us) sends the Receiving 
        #                 Server (them) a stream header - with a session id
        #  We don't have enough info to create a session, so we store the
        #  id on the channel
        set channel(id) [string map {- {}} [uuid::uuid generate]]

        set xml "<?xml version='1.0' encoding='utf-8'?>"
        append xml "<stream:stream xmlns='[xmlns server]'\
            xmlns:db='[xmlns dialback]' xmlns:stream='[xmlns stream]'\
            id='$channel(id)' version='1.0'>"

        # RFC3920 4.6: Stream Features
        if {$attr(version) >= 1.0} {
            append xml "<stream:features>"
            # FIX ME: provide tls support then add the feature here
            append xml "</stream:features>"
        }
        WriteTo $Channel $xml
    }
}

proc ::xmppd::s2s::OnCloseStream {Channel} {
    upvar #0 $Channel channel

    foreach Session [FindSession channel $Channel] {
        Log debug "closed session $Session"
        unset $Session
    }

    catch {close $channel(sock)}
    wrapper::reset $channel(parser)
    catch {unset channel} msg
    Log notice "- $Channel closed: $msg"
}

proc ::xmppd::s2s::OnError {Channel code args} {
    Log error "- $Channel error $code"
    WriteTo $Channel "</stream:stream>"
    OnCloseStream $Channel
}

proc ::xmppd::s2s::OnInput {Channel xmllist} {
    variable options
    upvar #0 $Channel channel

    Log debug "- Input $xmllist"
    foreach {cmd attr close value children} $xmllist break
    array set a {xmlns {} from {} to {}}
    array set a $attr

    switch -exact -- $cmd {
        features {
            Log debug "- features $xmllist"
        }
        result {

            # RFC3920 8.3: All stanzas MUST include both to and from
            if {$a(from) eq "" || $a(to) eq ""} {
                Raise $Channel improper-addressing
            }

            if {$a(xmlns:db) eq [xmlns dialback]} {
                
                if {[info exists a(type)]} {
                    # RFC3920 8.3(10): The Receiving Server (them) informs the 
                    #                  Originating Server (us)of the result.
                    set Session [FindSession name $a(from) $a(to)]
                    if {$Session eq {}} { 
                        return [Raise $Channel invalid-from]
                    }
                    upvar #0 $Session session
                    set session(state) $a(type)
                    return
                }

                # RFC3290 8.3(4): The Originating Server (them) sends a 
                #                 dialback key to the Receiving Server (us)
                #
                if {![info exists channel(id)]} {
                    Log error "Argh - no channel id!!"
                    return
                }
                set Session [CreateSession]
                upvar #0 $Session session
                set session(id)      $channel(id)     
                set session(state)   dialback
                set session(channel) $Channel
                set session(from)    $a(from)
                set session(to)      $a(to)
                set session(key)     $value

                # We need to send this key on the out channel with the 
                # out session id, from and to.
                set Out [FindSession name $a(to) $a(from)]
                if {$Out ne {}} {
                    upvar #0 $Out out
                    set xml "<db:verify xmlns:db='[xmlns dialback]'\
                            from='$a(to)' to='$a(from)'\
                            id='$session(id)'>$session(key)</db:verify>"
                    WriteTo $out(channel) $xml
                } else {
                    Log debug "- Creating new out channel to $a(from)"
                    Open $a(to) $a(from)
                }

            } else {
                Log error "unespected 'result' namespace'"
            }
        }
        verify {
            Log debug "- verify $xmllist" 
            
            # RFC3920 8.3: All stanzas MUST include both to and from
            if {$a(from) eq "" || $a(to) eq ""} {
                Raise $Channel improper-addressing
            }
            
            set Session [FindSession id $a(id)]
            if {$Session eq {}} { 
                # Raise invalid-id ??
                Log error "Failed to find session for '$a(id)'"
                return
            }
            upvar #0 $Session session
            if {$session(from) eq {}} {
                set session(from) $a(from)
                set session(to)   $a(to)
            }

            if {![info exists a(type)]} {
                
                # RFC3920 8.3(8): The Receiving Server (them) sends the 
                #                 Authoritative Server (us) a request for 
                #                 verification of a key. This is the id we
                #                 recieved in step 3 and its key. So we are
                #                 validating the out channel using data
                #                 recieved on the in channel.
                # Lets check the logic
                if {$Channel eq $session(channel)} {
                    Log error "LOGIC FAILURE"
                }
                # RFC 3920 8.3(9): Check the key against the out session
                set session(state) invalid
                if {$session(key) eq $value} {
                    set session(state) valid
                    Flush $Session
                }
                set xml "<db:verify xmlns:db='[xmlns dialback]'\
                       from='$session(from)' to='$session(to)'\
                       id='$session(id)' type='$session(state)'/>"
                WriteTo $Channel $xml

            } else {
                
                # RFC3920 8.3(9): The Authoritative Server (them) verifies the
                #                 valididy of the key and posts a message to
                #                 the Recieving Server (us).
                set session(state) $a(type)
                if {$session(state) eq "valid"} {

                    set Peer [FindSession name $a(to) $a(from)]
                    if {$Peer ne {}} {
                        upvar #0 $Peer peer

                        Log debug "* sess: [array get session]"
                        Log debug "* peer: [array get peer]"

                        set xml "<db:result xmlns:db='[xmlns dialback]'\
                            from='$peer(from)' to='$peer(to)'\
                            type='$a(type)'/>"

                        WriteTo $session(channel) $xml
                    } else {
                        # We need to create an outbound connection to go with
                        # this.
                        #Open $a(to) $a(from)
                        # IMPOSSIBLE??
                        Log error "ARGH: 8.3(10) this isnt supposed to happen"
                    }
                    
                } else {
                    Close $Channel
                }
            }
        }
        
        iq -
        message -
        presence {
            set domain [jid domain $a(to)]
            if {$domain eq [cget -domain]} {
                xmppd::route $a(from) $a(to) [wrapper::createxml $xmllist]
            } else {
                # error I should think unless we have components
                if {[set handler [cget -s2s:handler]] ne {}} {
                    eval $handler $xmllist
                } else {
                    Log error "No handler defined for \"$cmd\" stanzas"
                }
            }
        }

        default {
            Log debug "- event $xmllist"
        }
    }
}

# -------------------------------------------------------------------------

if {[llength [info commands ::xmppd::register]] > 0} {
    ::xmppd::register module xmppd::s2s
}

package provide xmppd::s2s $::xmppd::s2s::version

# -------------------------------------------------------------------------
