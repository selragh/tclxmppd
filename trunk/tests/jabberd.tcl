# jabbberd.tcl - Copyright (C) 2006 Pat Thoyts <patthoyts@users.sourceforge.net>
#
# Sample Jabber server.
#
# This aims to test out the tclxmppd framework by making it possible
# to try various Jabber clients against this implementation.
# We will work towards a full RFC3920 and RFC3921 compliant framework.
#
# $Id$

set auto_path [linsert $auto_path 0 \
                   [file dirname [file dirname [file normalize [info script]]]]]

package require xmppd::core
package require xmppd::s2s
package require xmppd::s2c
package require xmppd::sm

# handler gets called with the xmllist from wrapper.
proc Handler {xmllist} {
    array set a [list to [xmppd::cget -domain] from {} id {}]
    array set a [wrapper::getattrlist $xmllist]

    switch -exact -- [set type [wrapper::gettag $xmllist]] {
        iq {
            # RFC3921 3: Session Establishment
            set sx [wrapper::getchildswithtagandxmlns $xmllist \
                        session [xmppd::xmlns session]]
            if {[llength $sx] > 0} {
                # FIX ME: create a Jabberd session for this connected resource
                #         we can return an error here or disconnect a previous
                #         session. Do a 'sm' module for this?
                if {[catch {
                    set Session [xmppd::sm::CreateSession $a(from)]
                    set r [list iq [list type result to $a(from) from $a(to) id $a(id)] 1 {} {}]
                } err]} {
                    set rc {}
                    lappend rc [list session [list xmlns [xmppd::xmlns session]] 1 {} {}]
                    lappend rc [list error {type wait} 0 {} \
                                    [list [list internal-server-error [list xmlns [xmppd::xmlns stanzas]] 1 {} {}]]]
                    set r [list iq [list type error to $a(from) from $a(to) id $a(id)] 1 {} $rc]
                }
                    xmppd::route $a(to) $a(from) [wrapper::createxml $r]
                return
            }

            set xml "<iq xmlns='jabber:client' id='$a(id)' to='$a(from)'\
                from='$a(from)' type='error'><error\
                xmlns='[xmppd::xmlns stanzas]'/></iq>"
            xmppd::route $a(to) $a(from) $xml
        }
        
        presence {
            set Session [xmppd::sm::FindSession jid $a(from)]
            if {[llength $Session] > 0} {
                set Session [lindex $Session 0]
                upvar #0 $Session session
                # Initial presence - feed to sm for broadcast etc
                # - should be an sm method.
                if {$session(state) eq "active"} {
                    set session(state) available
                }
                set ps [lindex [wrapper::getchildswithtag $xmllist show] 0]
                if {$ps ne {}} {
                    set session(show) [wrapper::getcdata $ps]
                }
                set ps [lindex [wrapper::getchildswithtag $xmllist priority] 0]
                if {$ps ne {}} {
                    set priority [wrapper::getcdata $ps]
                    if {[string is integer $priority]} {
                        set session(priority) $priority
                    }
                }
            } else {
                Log debug "Hp $xmllist"
            }
        }

        message {
            Log debug "Hm $xmllist"
        }

        default {
            Log debug "Hd $xmllist"
        }
    }
}

proc Log {level msg} { puts stderr "$level: $msg" }

proc LoadConfig {} {
    # FIX ME: should load from a .conf file
    set cert [file join [file dirname [info script]] jabberd.pem]
    set db [file join [file dirname [info script]] jabberd.db]
    xmppd::configure  \
        -domain patthoyts.tk   \
        -loglevel debug    \
        -logfile xmppd.log   \
        -certfile $cert \
        -keyfile $cert  \
        -s2c:handler ::Handler \
        -s2c:authenticate ::xmppd::sm::authuser \
        -sm:db:type Sqlite \
        -sm:database $db
    
    xmppd::register feature session [xmppd::xmlns session]
}

proc start {} {
    ::xmppd::s2s::start
    ::xmppd::s2c::start
    ::xmppd::sm::start
}

proc stop {} {
    ::xmppd::sm::stop
    ::xmppd::s2c::stop
    ::xmppd::s2s::stop
}

# -------------------------------------------------------------------------

proc Main {} {
    global tcl_platform tcl_interactive tcl_service tk_version
    LoadConfig

    # Setup control stream.
    if {$tcl_platform(platform) eq "unix"} {
        set cmdloop [file join [file dirname [info script]] .. cmdloop.tcl]
        puts "Load $cmdloop"
        if {[file exists $cmdloop]} {
            source $cmdloop
            set cmdloop::welcome "Tcl XMPPD Test Server"
            append cmdloop::welcome "\nReady for input from %client %port"
            cmdloop::cmdloop
            set cmdloop::hosts_allow {127.0.0.1 ::1}
            cmdloop::listen 0.0.0.0 5448;# could do 0.0.0.0 5441
        }
        set tcl_interactive 1; # fake it so we can re-source this file
    }

    # Begin the component
    start

    # Loop forever, dealing with Wish or Tclsh
    if {[info exists tk_version]} {
        if {[tk windowingsystem] eq "win32"} { console show }
        wm withdraw .
        tkwait variable ::forever
        stop
    } else {
        # Permit running as a Windows service.
        if {![info exists tcl_service]} {
            vwait ::forever
            stop
        }
    }
}

if {!$tcl_interactive} {
    set r [catch [linsert $argv 0 ::Main] err]
    if {$r} {puts $errorInfo}
    exit $r
}

# -------------------------------------------------------------------------
# Local variables:
#   mode: tcl
#   indent-tabs-mode: nil
# End:
