#!/usr/bin/env tclsh
# Copyright (C) 2005 Pat Thoyts <patthoyts@users.sourceforge.net>
#
# A demo Jabber component.
#
#
# -------------------------------------------------------------------------
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.
# -------------------------------------------------------------------------

set auto_path [linsert $auto_path 0 [file dirname [file dirname [info script]]]]
package require xmppd::jcp;             # tclxmppd
package require xmppd::wrapper;         # tclxmppd

namespace eval ::component {
    variable version 1.0.0
    variable rcsid {$Id$}

    variable Options
    if {![info exists Options]} {
        array set Options {
            JID            {}
            Name           TestBridge
            Resource       testbridge
            Conference     {}
    
	    JabberServer   {}
            JabberPort     5347
            Secret         {}

            LogLevel       notice
            LogFile        {}
        }
    }
    
    variable Component
    variable NS
    array set NS {
        discoinfo  "http://jabber.org/protocol/disco#info"
        discoitems "http://jabber.org/protocol/disco#items"
        muc        "http://jabber.org/protocols/muc"
    }
}

# component::start --
#
#	Start the component. We create the JCP link. A successful link
#	will result in a call to the -handler function from where we
#	can perform further setup over the valid link
#
proc ::component::start {} {
    variable Options
    variable Component
    variable NS
    set Component [xmppd::jcp::create \
                       -component $Options(JID) \
                       -secret    $Options(Secret) \
                       -server    $Options(JabberServer) \
                       -port      $Options(JabberPort) \
                       -loglevel  $Options(LogLevel) \
                       -connectproc  [namespace origin OnConnect] \
                       -messageproc  [namespace origin OnMessage] \
                       -presenceproc [namespace origin OnPresence] \
                       -iqproc       [namespace origin OnIq]]
    $Component iq_register get jabber:iq:version \
        [namespace code [list OnIqVersion $Component]]
    $Component iq_register get $NS(discoinfo) \
        [namespace code [list OnIqDiscoInfo $Component]]
    # presence_register / message_register ?
    $Component connect
    component start
    return $Component
}

# component::stop --
#
#	Halt the component. We disconnect from the configured chat
#	by sending a presence unavailable and then destroy the component.
#
proc ::component::stop {} {
    variable Options
    variable Component
    component stop
    set jid "$Options(Name)@$Options(JID)/$Options(Resource)"
    presence $jid {} unavailable
    xmppd::jcp::destroy $Component
}

# component::OnConnect --
#
#	Jabber message routing. For this component, we don't need to
#	do anything as all we do is issue a time message on the hour.
#
proc ::component::OnConnect {xmllist} {
    variable Options

    # initial presence from the bridge client
    presence "$Options(Name)@$Options(JID)/$Options(Resource)"
}

proc ::component::OnMessage {xmllist} {
    array set a [linsert [wrapper::getattrlist $xmllist] 0 type normal]
    switch -exact -- $a(type) {
        groupchat -
        chat -
        normal -
        headline {
            set body [wrapper::getfirstchildwithtag $xmllist body]
            set text [wrapper::getcdata $body]
            puts "<$a(from)> $text"
        }
        default {
            puts stderr "unrecognised message type \"$a(type)\""
        }
    }
    return
}

proc ::component::OnPresence {xmllist} {
    array set a [linsert [wrapper::getattrlist $xmllist] 0 type available]
    switch -exact -- $a(type) {
        available {
            puts "$a(from) entered"
        }
        unavailable {
            puts "$a(from) left"
        }
        subscribe {
            # always refuse subsription requests
            presence $a(to) $a(from) unsubscribed
        }
        subscribed - unsubscribe - unsubscribed - probe - error {
            
        }
    }
    return
}

# component::OnIq --
#
#	iq get stanza handling
#
proc ::component::OnIq {xmllist} {
    return
}

# iq handler for jabber:iq:version (xep-0092)
proc ::component::OnIqVersion {Component xmllist} {
    variable version
    variable Options
    array set a [linsert [wrapper::getattrlist $xmllist] 0 id 0]
    lappend parts [wrapper::createtag name -chdata $Options(Name)]
    lappend parts [wrapper::createtag version -chdata $version]
    lappend parts [wrapper::createtag os -chdata "Tcl/[info patchlevel]"]
    lappend child [wrapper::createtag query -subtags $parts \
                       -attrlist {xmlns jabber:iq:version}]
    set rx [wrapper::createtag iq -subtags $child \
                -attrlist [list xmlns jabber:client type result id $a(id) \
                               to $a(from) from $a(to)]]
    $Component route [wrapper::createxml $rx]
    return -code break
}

# iq handler for urn:xmpp:time (xep-0202)
proc ::component::OnIqTime {Component xmllist} {
    variable version
    variable Options
    array set a [linsert [wrapper::getattrlist $xmllist] 0 id 0]
    set xep0082fmt "%Y-%m-%dT%H:%M:%SZ"
    set time [clock format [clock seconds] -format $xep0082fmt -gmt 1]
    set tzo [clock format [clock seconds] -format "%z" -gmt 0]
    lappend parts [wrapper::createtag utc -chdata $time]
    lappend parts [wrapper::createtag tzo -chdata $tzo]
    lappend child [wrapper::createtag time -subtags $parts \
                       -attrlist {xmlns urn:xmpp:time}]
    set rx [wrapper::createtag iq -subtags $child \
                -attrlist [list xmlns jabber:client type result id $a(id) \
                               to $a(from) from $a(to)]]
    $Component route [wrapper::createxml $rx]
    return -code break
}

# iq handler for service discovery
proc ::component::OnIqDiscoInfo {Component xmllist} {
    variable version
    variable Options
    variable NS

    array set a [linsert [wrapper::getattrlist $xmllist] 0 id 0]
    lappend parts [wrapper::createtag identity \
                       -attrlist [list name $Options(Name) \
                                      type testbridge \
                                      category service]]
    lappend parts [wrapper::createtag feature -attrlist [list var iq]]
    lappend parts [wrapper::createtag feature -attrlist [list var message]]
    lappend parts [wrapper::createtag feature -attrlist [list var $NS(discoinfo)]]
    lappend parts [wrapper::createtag feature -attrlist [list var $NS(discoitems)]]
    lappend parts [wrapper::createtag feature -attrlist [list var jabber:iq:version]]
    lappend parts [wrapper::createtag feature -attrlist [list var urn:xmpp:time]]
    lappend child [wrapper::createtag query -subtags $parts \
                       -attrlist [list xmlns $NS(discoinfo)]]
    set rx [wrapper::createtag iq -subtags $child \
                -attrlist [list xmlns jabber:client type result id $a(id) \
                               to $a(from) from $a(to)]]
    $Component route [wrapper::createxml $rx]
    return -code break
}

# component::presence --
#
#	Send a jabber presence message
#
proc ::component::presence {from {to {}} {type {}} {show {}} {status {}}} {
    variable Component
    variable Options
    variable NS
    set kids {}
    if {$show ne {}} {
        lappend kids [wrapper::createtag show -chdata $show]
    }
    if {$status ne {}} {
        lappend kids [wrapper::createtag status -chdata $status -attrlist {xml:lang en}]
    }
    set attr [list xmlns jabber:client from $from]
    if {$to ne {}} { lappend attr to $to }
    if {$type ne {}} { lappend attr type $type }
    
    $Component route [wrapper::createxml [wrapper::createtag presence \
                                              -subtags $kids -attrlist $attr]]
    return
}

# component::LoadConfig --
#
#	This procedure reads a text file and updates the Options array
#	from the contents. Comments and blank lines are ignored. All 
#	other lines must be a list of two elements, the first element 
#	must be an item in the Options array.
#
proc ::component::LoadConfig {{conf {}}} {
    variable Options
    if {$conf eq {}} {
        set conf [file normalize [info script]]
        set base [file rootname [file tail $conf]].conf
        set conf [file join [file dirname $conf] $base]
    }
    if {[file exists $conf]} {
        set f [open $conf r]
        set n 0
        while {![eof $f]} {
            gets $f line
            string trim $line
            if {[string match "#*" $line]} continue
            if {[string length $line] < 1} continue
            if {[llength $line] != 2} {
                return -code error "invalid config line $n: \"$line\""
            }
            if {![info exists Options([lindex $line 0])]} {
                return -code error "invalid config option\
                \"[lindex $line 0]\" at line $n"
            }
            set Options([lindex $line 0]) [lindex $line 1]
            incr n
        }
        close $f
    } else {
        return -code error "configuration file \"$conf\" could not be opened"
    }
    return
}

# component::component --
#
#	The implementation of this component.
#
proc ::component::component {cmd} {
    switch -exact -- $cmd {
        start {

        }
        stop {

        }
        default {
            return -code error "invalid option \"$cmd\": rtfm"
        }
    }
}

# -------------------------------------------------------------------------
# wubchain is using:
#  irc_send msg
#  irc_post nick msg : calls irc_send after /me handling
#  irc_recv          : receives msg, add to history, input is gets $fd line
#
#  On startup, hook up web interface and create component and join MUC
#  OnMessage: groupchat messages are to go into history
#             normal messages are memos to a specific user
#             chat messages are one-to-one chat messages
#  OnPresence: manage channel users arriving and departing
#  OnIq: queries - should be standard responses
#
#  When a user logs in, send a presence online to the MUC for
#  username@component.tclers.tk/nick
#  When they leave, send a presence unavailable for this jid.
#

# component::JoinMUC --
#
#	Join a MUC by sending a suitable presence to our desired nick jid.
#
proc JoinMUC {from conference nick} {
    variable ::component::Component
    variable ::component::NS

    lappend hist [wrapper::createtag history -attrlist {maxchars 0 maxstanzas 0}]
    lappend kids [wrapper::createtag x -attrlist [list xmlns $NS(muc)] -subtags $hist]
    set attr [list from $from to $conference/$nick xmlns jabber:client]
    $Component route [wrapper::createxml \
                          [wrapper::createtag presence -subtags $kids -attrlist $attr]]
    return
}

proc /join {nick} {
    variable ::component::Component
    variable ::component::Options
    set userjid $nick@$Options(JID)/webchat
    set nickjid $Options(Conference)/$nick
    ::component::presence $userjid
    ::component::presence $userjid $nickjid
}
proc /part {nick} {
    variable ::component::Component
    variable ::component::Options
    set userjid $nick@$Options(JID)/webchat
    set nickjid $Options(Conference)/$nick
    ::component::presence $userjid $nickjid unavailable
    ::component::presence $userjid {} unavailable
}
proc /post {nick message} {
    variable ::component::Component
    variable ::component::Options
    set userjid $nick@$Options(JID)/webchat
    set nickjid $Options(Conference)/$nick

    lappend body [wrapper::createtag body -chdata $message]
    set xmllist [wrapper::createtag message \
                     -attrlist [list xmlns jabber:client type groupchat \
                                    to $Options(Conference) from $userjid]\
                     -subtags $body]
    $Component route [wrapper::createxml $xmllist]
}

# -------------------------------------------------------------------------

proc ::component::Main {} {
    global tcl_platform tcl_interactive tcl_service tk_version
    variable Options
    LoadConfig

    # Setup control stream.
    if {$tcl_platform(platform) eq "unix"} {
        set cmdloop [file join [file dirname [info script]] cmdloop.tcl]
        if {[file exists $cmdloop]} {
            puts "Loading $cmdloop"            
            source $cmdloop
            set cmdloop::welcome "$Options(Name) v[set [namespace current]::version]"
            append cmdloop::welcome "\nReady for input from %client %port"
            cmdloop::cmdloop
            #set cmdloop::hosts_allow {127.0.0.1 ::1}
            #cmdloop::listen 127.0.0.1 5442;# could do 0.0.0.0 5441
        } else {
            puts "Command loop not available."
        }
        set tcl_interactive 1; # fake it so we can re-source this file
    }

    # Begin the component
    start

    # Loop forever, dealing with wish, tclsh or tclsvc
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
    set r [catch [linsert $argv 0 ::component::Main] err]
    if {$r} {puts $errorInfo}
    exit $r
}
