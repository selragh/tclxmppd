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

    variable options
    if {![info exists options]} {
        array set options {
            component  component.example.com
            secret     secret
            loglevel   debug
            handler    {}
        }
    }


    variable log
    if {![info exists log]} {
        set log [logger::init jcp]
        ${log}::setlevel $options(loglevel)
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
    
}

proc ::xmppd::jcp::configure {args} {
    variable options
    variable log
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
            -component { 
                if {$cget} {
                    return $options(component)
                } else {
                    set options(component) [Pop args 1]
                }
            }
            -secret {
                if {$cget} {
                    return $options(secret)
                } else {
                    set options(secret) [Pop args 1]
                }
            }
            -loglevel {
                if {$cget} {
                    return $options(loglevel)
                } else {
                    set options(loglevel) [Pop args 1]
                    ${log}::setlevel $options(loglevel)
                }
            }
            -handler {
                if {$cget} {
                    return $options(handler)
                } else {
                    set options(handler) [Pop args 1]
                }
            }
            -- { Pop args ; break }
            default {
                set opts [join [lsort [array names options]] ", -"]
                return -code error "bad option \"$option\":\
                    must be one of -$opts"
            }
        }
        Pop args
    }
    return
}

#
# component::join target as me
proc ::xmppd::jcp::create {server {port 5347}} {
    variable options
    set sock [socket -async $server $port]
    set id [namespace current]::[string map {sock jcp} $sock]
    upvar #0 $id state
    set state(sock) $sock
    set state(server) $server
    set state(component) $options(component)
    set state(parser) [wrapper::new \
                           [list [namespace current]::OnOpenStream $id] \
                           [list [namespace current]::OnCloseStream $id] \
                           [list [namespace current]::OnInput $id] \
                           [list [namespace current]::OnError $id]]
    fconfigure $sock -buffering none -blocking 0 \
        -encoding utf-8 -translation lf
    fileevent $sock writable [list [namespace current]::Write $id]
    fileevent $sock readable [list [namespace current]::Read $id]
    return $id
}

proc ::xmppd::jcp::destroy {Component} {
    upvar #0 $Component state
    WriteTo $state(sock) "</stream:stream>"
    OnCloseStream $Component
    return
}

proc ::xmppd::jcp::route {Component msg} {
    upvar #0 $Component state
    WriteTo $state(sock) $msg
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
    variable options
    upvar #0 $Component state
    Log debug "OPEN  $Component $args"
    array set a $args
    if {[info exists a(id)]} {
        # JEP0114 3 (2): Server replies with stream header plus stream id.
        #                We must reply with the handshake hash.
        set state(streamid) $a(id)
        set reply [sha1::sha1 $state(streamid)$options(secret)]
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
    variable options
    upvar #0 $Component state
    #Log debug "INPUT $Component $xmllist"

    array set a {xmlns {} from {} to {}}
    array set a [wrapper::getattrlist $xmllist]

    switch -exact -- [set tag [wrapper::gettag $xmllist]] {
        features {
            Log notice "? features $xmllist"
        }
        result {
            Log notice "? result $xmllist"
        }
        verify {
            Log notice "? verify $xmllist"
        }
        handshake -
        iq -
        message -
        presence {
            if {$options(handler) ne {}} {
                if {[catch {$options(handler) $xmllist} err]} {
                    Log error "! error handing \"$tag\" stanza: $err"
                }
            } else {
                Log error "! No handler defined for \"$tag\" stanzas"
            }
        }
        default {
            Log notice "- unrecognized stanza: $xmllist"
        }
    }
}

# -------------------------------------------------------------------------

package provide xmppd::jcp $::xmppd::jcp::version

# -------------------------------------------------------------------------
# Local variables:
#   mode: tcl
#   indent-tabs-mode: nil
# End:
