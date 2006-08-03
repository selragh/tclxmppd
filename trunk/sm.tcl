# sm.tcl - Copyright (C) 2004 Pat Thoyts <patthoyts@users.sourceforge.net>
#
# XMPP IM session manager.
#
# This module covers management of instant messaging session, roster and 
# XMPP subscription management.
#
# -------------------------------------------------------------------------
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.
# -------------------------------------------------------------------------

package require xmppd::core;            # tclxmppd 
package require DIO;                    # Rivet database access library

namespace eval ::xmppd {
    namespace eval sm {
        variable version 1.0.0
        variable rcsid {$Id$}
        
        #namespace export 
        
        variable options
        if {![info exists options]} {
            array set options {
                sm:database ""
                sm:db:type Sqlite
                sm:db:host localhost
                sm:db:user ""
                sm:db:pass ""
            }
        }
        
        variable uid
        if {![info exists uid]} { set uid 0 }
        
        namespace import -force ::xmppd::configure ::xmppd::cget \
            ::xmppd::Pop ::xmppd::xmlns ::xmppd::jid ::xmppd::Log
    }
}

# s2c channels have a jid and a resource item. However the channel could get closed
# underneath the session (maybe).
#
# Sessions are tied to active resurces - that means the JID MUST have a resource.
#
# state: active (after session establishment) available (after initial presence)
# show: one of dnd chat
#
proc ::xmppd::sm::CreateSession {jid} {
    # Find the s2c channel corresponding to the JID in question
    set Channel {}
    set resource [jid resource $jid]
    foreach chan [xmppd::s2c::FindChannel $jid] {
        if {[info exists [set chan](resource)] \
                && [string equal [set [set chan](resource)] $resource]} {
            set Channel $chan
        }
    }
    if {[llength $Channel] != 1} {
        return -code error "invalid jid - no channel found"
    }

    variable uid
    set Session [namespace current]::session[incr uid]
    upvar #0 $Channel channel
    array set $Session [list state active preference 0 show {} status {} \
                            channel $Channel jid $channel(jid) resource $channel(resource)]

    if {[info exists channel(session)]} {
        # do something about it - we are replacing a session on the same channel?
    }
    set channel(session) $Session
    return $Session
}

proc ::xmppd::sm::ListSessions {} {
    set r {}
    foreach Session [info vars [namespace current]::session*] {
        upvar #0 $Session session
        lappend r [list [namespace tail $Session] $session(state) [namespace tail $session(Channel)]]
    }
    return $r
}

proc ::xmppd::sm::FindSession {op args} {
    set r {}
    switch -exact -- $op {
        jid {
            set jid [xmppd::jid !resource [lindex $args 0]]
            foreach Session [info vars [namespace current]::session*] {
                upvar #0 $Session session
                if {$session(jid) eq $jid} {
                    lappend r $Session
                }
            }
        }
        default {
            return -code error "invalid option \"$op\": must be one of jid"
        }
    }
    return $r
}

proc ::xmppd::sm::_configure {args} {
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
            -sm:database {
                if {$cget} {
                    return $options(sm:database)
                } else {
                    set options(sm:database) [Pop args 1]
                }
            }
            -sm:db:type {
                if {$cget} {
                    return $options(sm:db:type)
                } else {
                    set options(sm:db:type) [Pop args 1]
                }
            }
            -sm:db:host {
                if {$cget} {
                    return $options(sm:db:host)
                } else {
                    set options(sm:db:host) [Pop args 1]
                }
            }
            -sm:db:user {
                if {$cget} {
                    return $options(sm:db:user)
                } else {
                    set options(sm:db:user) [Pop args 1]
                }
            }
            -sm:db:pass {
                if {$cget} {
                    return $options(sm:db:pass)
                } else {
                    set options(sm:db:pass) [Pop args 1]
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

proc ::xmppd::sm::start {} {
    variable db
    if {![info exists db]} {
        set db [DIO::handle [cget -sm:db:type] [namespace current]::db \
                    -host [cget -sm:db:host] \
                    -user [cget -sm:db:user] \
                    -pass [cget -sm:db:pass] \
                    -db [cget -sm:database]]
        # Check for table already present
        set r [$db exec {SELECT COUNT(username) FROM authreg;}]
        if {[$r errorcode] != 0} {
            puts "Creating databases"
            set tables(authreg) {username VARCHAR(256),realm VARCHAR(256),
                password VARCHAR(256),token VARCHAR(10),sequence INTEGER, hash VARCHAR(40)}
            set tables(roster) {username VARCHAR(256),jid TEXT,state INTEGER}
            foreach table [array names tables] {
                set r [$db exec "CREATE TABLE $table ($tables($table));"]
                if {[$r errorcode] != 0} {
                    return -code error [$r errorinfo]
                }
            }
            set r [$db exec "CREATE INDEX idx_authreg ON authreg(username);"]
            if {[$r errorcode] != 0} {
                return -code error [$r errorinfo]
            }
        }
    }
    return
}

proc ::xmppd::sm::stop {} {
    variable db
    if {[info exists db]} {
        $db close
        rename [namespace current]::db {}
        unset db
    }
}

proc ::xmppd::sm::authuser {authid realm} {
    variable db
    if {![info exists db]} {
        return -code error "unexpected: xmppd::sm::start not called"
    }
    Log debug "Authenticating $authid $realm..."
    set r [$db exec "SELECT username,realm,password FROM authreg\
        WHERE username=[SqlQuote $authid] AND realm=[SqlQuote $realm];"]
    if {[$r errorcode]} {
        Log debug "... auth failure [$r errorinfo]"
        return -code error [$r errorinfo]
    } else {
        set res ""
        $r forall -array f {
            set res $f(password)
        }
    }
    return $res
}

proc ::xmppd::sm::SqlQuote {s} {return "'[string map {"'" "''"} $s]'"}

# -------------------------------------------------------------------------

if {[llength [info commands ::xmppd::register]] > 0} {
    ::xmppd::register module xmppd::sm
}

package provide xmppd::sm $::xmppd::sm::version

# -------------------------------------------------------------------------
