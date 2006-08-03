# ctalk.tcl - Copyright (C) 2006 Pat Thoyts <patthoyts@users.sourceforge.net>
#
# Simple Jabber text-mode client.
#
# This is a simple text-only XMPP client application. You need to have
# installed jlib (from the Coccinella project - as used by tkchat) and we
# need some tcllib modules too (SASL, md5, sha1). You might also want to
# have tls installed but this is not required.
#
# $Id$

package require SASL;                   # tcllib
package require tls;                    # tls
package require sha1;                   # tcllib
package require jlib;                   # jlib

namespace eval ctalk {
    variable version 1.0.0
    variable rcsid {$Id$}

    variable App
    if {![info exists App]} {
        array set App {
            user      patthoyts
            server    patthoyts.tk
            password  secret
            connect   localhost:5222
            resource  ctalk
            keepalive 10
        }
    }
}

proc ::ctalk::Log {msg} {
    set t [clock format [clock seconds] -format "%H:%M:%S"]
    puts "$t: $msg"
}

proc ::ctalk::Print {msg} {
    set t [clock format [clock seconds] -format "%H:%M:%S"]
    puts "$t: $msg"
}

proc ::ctalk::Connect {} {
    variable App
    set roster [::roster::roster [namespace origin RosterCallback]]
    set App(conn) [::jlib::new $roster [namespace origin ClientCallback] \
                       -iqcommand [namespace origin IqCallback] \
                       -messagecommand [namespace origin MessageCallback] \
                       -presencecommand [namespace origin PresenceCallback] \
                       -keepalivesecs $App(keepalive)]
    ::jlib::iq_register $App(conn) get jabber:iq:version \
        [namespace origin IqVersionCallback] 40


    foreach {host port} [split $App(connect) :] break
    if {$port eq {}} { set port 5222 }
    set sock [socket $host $port]
    $App(conn) setsockettransport $sock
    $App(conn) openstream $App(server) \
        -cmd [namespace origin ConnectCallback] \
        -socket $sock -version 1.0
}

proc ::ctalk::Stop {} {
    variable App
    $App(conn) closestream
}

proc ::ctalk::ConnectCallback {tok args} {
    variable App
    upvar ${tok}::lib lib
    fconfigure $lib(sock) -encoding utf-8
    jlib::auth_sasl $tok $App(user) $App(resource) $App(password) \
        [namespace origin LoginCallback]
}

proc ::ctalk::LoginCallback {tok type msg} {
    switch -- $type {
        result {
            # RFC3921:5.1.1 Initial presence (unless this is done by jlib)
            $tok send_presence
        }
        error {
            Log "# login $type $msg"
            Stop
        }
        default {
            Log "! undefined type \"$type\" in LoginCallback"
        }
    }
}

proc ::ctalk::ClientCallback {tok cmd args} {
    array set a {-body {} -errormsg {}}
    array set a $args
    switch -- $cmd {
        connect {
            Log "* Connected"
        }
        disconnect {
            Log "* Disconnect"
            # cleanup and schedule reconnect
        }
        networkerror {
            Log "* Network error: $a(-body)"
            #cleanup and schedule reconnect
        }
        streamerror {
            Log "* Stream error: $a(-errormsg)"
            # exit
        }
        default {
            Log "* $cmd $args"
        }
    }
}

proc ::ctalk::RosterCallback {roster what {jid {}} args} {
    Log "= roster $what $jid $args"
}

proc ::ctalk::IqVersionCallback {tok from iq args} {
    variable version
    array set a {-id 0}
    array set a $args
    set ver [list [wrapper::createtag name -chdata "CTalk"] \
                 [wrapper::createtag version -chdata $version] \
                 [wrapper::createtag os -chdata "Tcl [info patchlevel]"]]
    set x [wrapper::createtag query -attrlist {xmlns jabber:iq:version} \
               -subtags $ver]
    jlib::send_iq $tok "result" [list $x] -id $a(-id) -to $from
    return 1
}

proc ::ctalk::PresenceCallback {tok type args} {
    if {[catch [linsert $args 0 PresenceCallback2 $tok $type] err]} {
        Log "Error: $err"
    }
}

proc ::ctalk::PresenceCallback2 {tok type args} {
    array set a {-from {} -to {} -status {}}
    array set a $args
    Log "< presence $type $a(-from) $a(-to) $a(-status)"
}

proc ::ctalk::IqCallback {tok type args} {
    if {[catch [linsert $args 0 IqCallback2 $tok $type] err]} {
        Log "Error: $err"
    }
}

proc ::ctalk::IqCallback2 {tok type args} {
    array set a {-from {} -to {}}
    array set a $args
    Log "< iq $type $a(-from) $a(-to)"
}

proc ::ctalk::MessageCallback {tok type args} {
    if {[catch [linsert $args 0 MessageCallback2 $tok $type] err]} {
        Log "Error: $err"
    }
}

proc ::ctalk::MessageCallback2 {tok type args} {
    array set a {-from {} -to {} -subject {} -body {}}
    array set a $args
    switch -exact -- $type {
        chat {
            Print "$a(-from) $a(-body)" 
        }
        normal {
            Print "$a(-from) \"$a(-subject)\"\n    $a(-body)" 
        }
        default {
            Log "< message $type $a(-from) $a(-to) $args"
        }
    }
}

proc ::ctalk::jid {part jid} {
    set r {}
    if {[regexp {^(?:([^@]*)@)?([^/]+)(?:/(.+))?} $jid \
        -> node domain resource]} {
        switch -exact -- $part {
            node      { set r $node }
            domain    { set r $domain }
            resource  { set r $resource }
            !resource { set r ${node}@${domain} }
            jid       { set r $jid }
            default {
                return -code error "invalid part \"$part\":\
                    must be one of node, domain, resource or jid."
            }
        }
    }
    return $r
}

interp alias {} jid {} ::ctalk::jid

proc say {to message} {
    variable ::ctalk::App
    if {[jid node $to] eq {}} {
        set to $to@$App(server)
    }
    $App(conn) send_message $to -type chat -body $message
}

proc ::ctalk::Main {} {
    global tcl_platform tcl_interactive tcl_service tk_version
    #LoadConfig

    # Setup control stream.
    if {$tcl_platform(platform) eq "unix"} {
        set cmdloop [file join [file dirname [info script]] .. cmdloop.tcl]
        puts "Load $cmdloop"
        if {[file exists $cmdloop]} {
            source $cmdloop
            set cmdloop::welcome "CTalk XMPP client"
            append cmdloop::welcome "\nReady for input"
            cmdloop::cmdloop
        }
        set tcl_interactive 1; # fake it so we can re-source this file
    }

    # Start the app
    Connect

    # Loop forever, dealing with Wish or Tclsh
    if {[info exists tk_version]} {
        if {[tk windowingsystem] eq "win32"} { console show }
        wm withdraw .
        tkwait variable ::forever
        Stop
    } else {
        # Permit running as a Windows service.
        if {![info exists tcl_service]} {
            vwait ::forever
            Stop
        }
    }
}

if {!$tcl_interactive} {
    set r [catch [linsert $argv 0 ::ctalk::Main] err]
    if {$r} {puts $errorInfo}
    exit $r
}