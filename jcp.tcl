# jcp.tcl - Copyright (C) 2006 Pat Thoyts <patthoyts@users.sourceforge.net>
#
# JEP-0114 - Jabber Component Protocol
#

package require xmppd::wrapper;         # tclxmppd
package require sha1;                   # tcllib
package require logger;                 # tcllib

namespace eval ::xmppd {}
namespace eval ::xmppd::jcp {
    variable version 1.1.0
    variable rcsid {$Id: jcp.tcl,v 1.2 2004/12/08 15:22:11 pat Exp $}
    variable uid; if {![info exists uid]} { set uid 0 }
    variable options
    if {![info exists options]} {
        array set options {
            component  component.example.com
            secret     secret
            loglevel   debug
            server     xmppd.example.com
            port       5347
            handler    {}
            connectproc {}
            disconnectproc {}
            messageproc {}
            presenceproc {}
            iqproc {}
        }
    }
}

# Create a component.
# We create a state array and matching command and call configure
# to initialize the settings. Then connect the component which will
# cause everything else to operate via the callbacks.
proc ::xmppd::jcp::create {args} {
    variable uid
    variable options
    set id [namespace current]::jcp[incr uid]
    upvar #0 $id state
    array set state [array get options]
    set state(log) [logger::init jcp]
    eval [linsert $args 0 configure $id]
    proc $id {cmd args} "eval \[linsert \$args 0 \$cmd $id\]"
    set state(parser) [wrapper::new \
                           [list [namespace current]::OnOpenStream $id] \
                           [list [namespace current]::OnCloseStream $id] \
                           [list [namespace current]::OnInput $id] \
                           [list [namespace current]::OnError $id]]
    iq_register $id get default [namespace code [list OnIqDefault $id]] 1000
    iq_register $id set default [namespace code [list OnIqDefault $id]] 1000
    
    return $id
}

proc ::xmppd::jcp::connect {Component} {
    upvar #0 $Component state
    set state(sock) [socket -async $state(server) $state(port)]
    fconfigure $state(sock) -buffering none -blocking 0 \
        -encoding utf-8 -translation lf
    fileevent $state(sock) writable [list [namespace current]::Write $Component]
    fileevent $state(sock) readable [list [namespace current]::Read $Component]
}

proc ::xmppd::jcp::configure {Component args} {
    upvar #0 $Component state
    variable log
    if {[llength $args] < 1} {
        set r {}
        foreach opt [lsort [array names state]] {
            lappend r -$opt $state($opt)
        }
        return $r
    }

    set cget [expr {[llength $args] == 1 ? 1 : 0}]
    while {[string match -* [set option [lindex $args 0]]]} {
        switch -glob -- $option {
            -component -
            -secret -
            -server -
            -port -
            -connectproc -
            -disconnectproc -
            -messageproc -
            -iqproc -
            -presenceproc { 
                set option [string trimleft $option -]
                if {$cget} {
                    return $state($option)
                } else {
                    set state($option) [Pop args 1]
                }
            }
            -loglevel {
                if {$cget} {
                    return $state(loglevel)
                } else {
                    set state(loglevel) [Pop args 1]
                    set log $state(log)
                    ${log}::setlevel $state(loglevel)
                }
            }
            -- { Pop args ; break }
            default {
                variable options
                set opts [join [lsort [array names options]] ", -"]
                return -code error "bad option \"$option\":\
                    must be one of -$opts"
            }
        }
        Pop args
    }
    return
}

proc ::xmppd::jcp::destroy {Component} {
    upvar #0 $Component state
    WriteTo $state(sock) "</stream:stream>"
    OnCloseStream $Component
    return
}

proc ::xmppd::jcp::iq_register {Component type xmlns cmd {priority 50}} {
    Hook $Component add iq $type $xmlns $cmd $priority
}

proc ::xmppd::jcp::route {Component msg} {
    upvar #0 $Component state
    WriteTo $state(sock) $msg
}

proc ::xmppd::jcp::Hook {Component do type args} {
    upvar #0 $Component state
    set valid {message presence iq}
    if {[lsearch -exact $valid $type] == -1} {
        return -code error "unknown hook type \"$type\":\
                must be one of [join $valid ,]"
    }
    if {$type eq "iq"} {
        set default iq,[lindex $args 0],default
        set type iq,[join [lrange $args 0 1] ","]
        set args [lrange $args 2 end]
    } else {
        set default $type,default
    }
    switch -exact -- $do {
	add {
            if {[llength $args] < 1 || [llength $args] > 2} {
                return -code error "wrong # args: should be \"add hook cmd ?priority?\""
            }
            foreach {cmd pri} $args break
            if {$pri eq {}} { set pri 50 }
            lappend state(hook,$type) [list $cmd $pri]
            set state(hook,$type) [lsort -real -index 1 [lsort -unique $state(hook,$type)]]
	}
        remove {
            if {[llength $args] != 1} {
                return -code error "wrong # args: should be \"remove hook cmd\""
            }
            if {![info exists state(hook,$type)]} { return }
            for {set ndx 0} {$ndx < [llength $state(hook,$type)]} {incr ndx} {
                set item [lindex $state(hook,$type) $ndx]
                if {[lindex $item 0] eq [lindex $args 0]} {
                    set state(hook,$type) [lreplace $state(hook,$type) $ndx $ndx]
                    break
                }
            }
            set state(hook,$type)
        }
        run {
            set hooks {}
            if {[info exists state(hook,$type)]} {
                set hooks $state(hook,$type)
            }
            if {[info exists state(hook,$default)]} {
                set hooks [concat $hooks $state(hook,$default)]
            }
            if {[llength $hooks] < 1} { return }
            set res ""
            foreach item $hooks {
                foreach {cmd pri} $item break
                set code [catch {eval $cmd $args} err]
                if {$code == 0} {                   ;# ok
                    lappend res $err
                } elseif {$code == 1 || $code == 3} { ;# error, break
                    set ::ERR $::errorInfo
                    return -code $code -errorcode $::errorCode -errorinfo $::errorInfo $err
                }
            }
            return $res
        }
        list {
            if {[info exists state(hook,$type)]} {
                return $state(hook,$type)
            }
        }
	default {
	    return -code error "unknown hook action \"$do\":\
                must be add, remove, list or run"
	}
    }
}

proc ::xmppd::jcp::Log {level msg} { puts stderr $msg }
proc ::xmppd::jcp::SetLogLevel {Component} {
    upvar #0 $Component state
    set log $state(log)
    ${log}::setlevel $state(loglevel)
    namespace eval $log {
        variable logfile ""
        #set logfile [open s2s.log a+]
        #fconfigure $logfile -buffering line
        #puts $logfile [string repeat - 72]
    }
    proc ${log}::stdoutcmd {level text} {
        variable service
        variable logfile
        set ts [clock format [clock seconds] -format {%H:%M:%S}]
        if {$logfile != {}} {
            puts $logfile "\[$ts\] $level $text"
        }
        puts stderr $text
    }
    proc Log {level msg} {variable log; ${log}::${level} $msg}
}
    
#  Pop the nth element off a list. Used in options processing.
#
proc ::xmppd::jcp::Pop {varname {nth 0}} {
    upvar $varname args
    set r [lindex $args $nth]
    set args [lreplace $args $nth $nth]
    return $r
}

proc ::xmppd::jcp::WriteTo {chan data} {
    Log debug "> $chan $data"
    puts -nonewline $chan $data
}

proc ::xmppd::jcp::Write {Component} {
    upvar #0 $Component state
    fileevent $state(sock) writable {}
    set xml "<?xml version='1.0' encoding='utf-8'?>"
    append xml "<stream:stream xmlns='jabber:component:accept'"
    append xml " xmlns:stream='http://etherx.jabber.org/streams'"
    append xml " to='$state(component)'>"
    WriteTo $state(sock) $xml
}

proc ::xmppd::jcp::Read {Component} {
    upvar #0 $Component state
    if {[eof $state(sock)]} {
        fileevent $state(sock) readable {}
        Log notice "! $state(sock) END OF FILE"
        OnCloseStream $Component
        return
    }
    set xml [read $state(sock)]
    Log debug "< $state(sock) $xml"
    wrapper::parse $state(parser) $xml
}

proc ::xmppd::jcp::OnOpenStream {Component args} {
    upvar #0 $Component state
    Log debug "OPEN  $Component $args"
    array set a $args
    if {[info exists a(id)]} {
        # JEP0114 3 (2): Server replies with stream header plus stream id.
        #                We must reply with the handshake hash.
        set state(streamid) $a(id)
        set reply [sha1::sha1 $state(streamid)$state(secret)]
        set xml "<handshake>$reply</handshake>"
        WriteTo $state(sock) $xml
    } else {
        Log notice "?????????"
    }
}

proc ::xmppd::jcp::OnCloseStream {Component} {
    upvar #0 $Component state
    Log debug "CLOSE $Component"
    catch {close $state(sock)}
    wrapper::reset $state(parser)
    unset state
}

proc ::xmppd::jcp::OnErrorStream {Component code args} {
    upvar #0 $Component state
    Log debug "ERROR $Component $code $args"
    WriteTo $state(sock) "</stream:stream>"
    OnCloseStream $Component
}

proc ::xmppd::jcp::OnInput {Component xmllist} {
    upvar #0 $Component state
    #Log debug "INPUT $Component $xmllist"

    array set a {xmlns {} from {} to {} id {}}
    array set a [wrapper::getattrlist $xmllist]

    set handled 0
    switch -exact -- [set tag [wrapper::gettag $xmllist]] {
        features {
            Log notice "? features $xmllist"
            set handled 1
        }
        result {
            Log notice "? result $xmllist"
            set handled 1
        }
        verify {
            Log notice "? verify $xmllist"
            set handled 1
        }
        handshake {
            if {[info exists state(connectproc)] 
                && $state(connectproc) ne {}
            } then {
                if {[catch {$state(connectproc) $xmllist} err]} {
                    Log error "! error handling connectproc: $err"
                }
            }
        }
        iq {
            # RFC 3920 9.2.3: must have type attr. get,set,result,error
            #   get/set have 1 child, must reply
            #   result no reply, 0/1 childs
            #   error no reply, include get/set child + error child.
            set child [lindex [wrapper::getchildren $xmllist] 0]
            set ns [wrapper::getattr [wrapper::getattrlist $child] xmlns]
            set r [catch {Hook $Component run iq $a(type) $ns $xmllist} err]
            if {$r == 1} {
                set tag [wrapper::gettag $child]
                set rsp [RaiseIQ internal-server-error $xmllist $err]
                route $Component [wrapper::createxml $rsp]
            }
        }
        message -
        presence {
            set cmd ${tag}proc
            if {[info exists state($cmd)] && $state($cmd) ne {}} {
                if {[catch {$state($cmd) $xmllist} err]} {
                    Log error "! error handing \"$tag\" stanza: $err"
                }
            }
        }
        default {
            Log notice "- unrecognized stanza: $xmllist"
        }
    }
}

# ::xmppd::jcp::OnIqDefault --
#
#	Default iq get and iq set message handler.
#	Returns a not-implemented error.
#
proc ::xmppd::jcp::OnIqDefault {Component xmllist} {
    array set a [linsert [wrapper::getattrlist $xmllist] 0 id {}]
    set tag [wrapper::gettag [lindex [wrapper::getchildren $xmllist] 0]]
    set rsp [RaiseIQ feature-not-implemented $xmllist "This feature is not available"]
    route $Component [wrapper::createxml $rsp]
    return -code break
}

# ::xmppd::jcp::RaiseIQ --
#	
#	Raise an error response for invalid queries or for queries we do 
#	not intend to handle.
#	Returns an xmllist containing an iq error.
#
#proc ::xmppd::jcp::RaiseIQ {query type id self requester {text {}}} {
proc ::xmppd::jcp::RaiseIQ {errortype xmllist text} {
    array set a [linsert [wrapper::getattrlist $xmllist] 0 id {}]
    set firstchild [lindex [wrapper::getchildren $xmllist] 0]
    set tag [wrapper::gettag $firstchild]
    set tagns [wrapper::getattribute $firstchild xmlns]

    set ns urn:ietf:params:xml:ns:xmpp-stanzas
    lappend p [wrapper::createtag $errortype -attrlist [list xmlns $ns]]
    if {$text ne ""} {
        lappend p [wrapper::createtag text -chdata $text \
                       -attrlist [list xmlns $ns xml:lang en]]
    }
    lappend qr [wrapper::createtag $tag -attrlist [list xmlns $tagns]]
    lappend qr [list error {type cancel code 501} 0 {} $p]
    set ra [list xmlns jabber:client type error id $a(id) to $a(from) from $a(to)]
    set rsp [list iq $ra 0 {} $qr]
}

# -------------------------------------------------------------------------

package provide xmppd::jcp $::xmppd::jcp::version

# -------------------------------------------------------------------------
# Local variables:
#   mode: tcl
#   indent-tabs-mode: nil
# End:
