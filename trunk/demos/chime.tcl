#!/usr/bin/env tclsh
# chime.tcl - Copyright (C) 2005 Pat Thoyts <patthoyts@users.sourceforge.net>
#
# A demo Jabber component.
#
# This component connects to a multi-user chat and issues a time message on
# the hour each hour. It serves to illustrate how to create a component 
# using the tclxmppd jcp package.
#
# -------------------------------------------------------------------------
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.
# -------------------------------------------------------------------------

set auto_path [linsert $auto_path 0 \
                   [file join [file dirname [file dirname [info script]]]]]
package require xmppd::jcp;             # tclxmppd
package require xmppd::wrapper;         # jabberlib

namespace eval ::chime {
    variable version 1.0.0
    variable rcsid {$Id: chime.tcl,v 1.3 2006/04/13 11:50:31 pat Exp $}

    variable Options
    if {![info exists Options]} {
        array set Options {
            JID            {}
            Name           Chime
            Resource       chime
            Conference     {}
    
	    JabberServer   {}
            JabberPort     5347
            Secret         {}

            LogLevel       notice
            LogFile        {}
        }
    }
    
    variable Component
}

# chime::start --
#
#	Start the chime component. This uses the jabber component protocol
#	to connect to the server and schedules the chimes.
#	We join the chat by sending an appropriate presence message once
#	we are fully connected.
#
proc ::chime::start {} {
    variable Options
    variable Component
    xmppd::jcp::configure \
        -component $Options(JID) \
        -secret    $Options(Secret) \
        -loglevel  $Options(LogLevel) \
        -handler   [namespace current]::Handler
    set Component [xmppd::jcp::create \
                       $Options(JabberServer) $Options(JabberPort)]

    set jid "$Options(Name)@$Options(JID)/$Options(Resource)"
    set nick "$Options(Conference)/$Options(Name)"
    after 200 [list [namespace origin presence] $jid $nick \
                   available online {Hourly chime}]

    chimes start
    return
}

# chime::stop --
#
#	Halt the chime component. We disconnect from the configures chat
#	by sending a presence unavailable and then destroy the component.
#
proc ::chime::stop {} {
    variable Options
    variable Component
    chimes stop
    set jid "$Options(Name)@$Options(JID)/$Options(Resource)"
    set nick "$Options(Conference)/$Options(Name)"
    presence $jid $nick unavailable
    xmppd::jcp::destroy $Component
}

# chime::Handler --
#
#	Jabber message routing. For this component, we don't need to
#	do anything as all we do is issue a time message on the hour.
#
proc ::chime::Handler {type attributes close value children} {
    array set a {from {} to {} type {}}
    array set a $attributes

    switch -exact -- $type {
        message {}
        presence {}
        iq {
            switch -exact -- $a(type) {
                get {
                    foreach child $children {
                        if {[wrapper::gettag $child] eq "query"} {
                            HandleIQ $child $a(id) $a(to) $a(from)
                        }
                    }
                }
            }            
        }
        default {}
    }
    return
}

# chime::HandleIQ --
#
#	I am sure some of this could be factored into the component package.
#	Basically all components should register for some minimal IQ handling
#	just to provide their name and version if nothing else.
#
proc ::chime::HandleIQ {child id self requester} {
    variable Options
    variable Component
    variable version

    set query [wrapper::getattribute $child xmlns]
    set rsp {}
    set parts {}
    switch -exact -- $query {
        jabber:iq:version {
            lappend parts [list name {} 0 $Options(Name) {}]
            lappend parts [list version {} 0 $version {}]
            lappend parts [list os {} 0 "Tcl/[info patchlevel]" {}]
            lappend qr [list query [list xmlns $query] 0 {} $parts]
            set ra [list xmlns jabber:client type result id $id \
                to $requester from $self]
            set rsp [list iq $ra 0 {} $qr]
        }
        "http://jabber.org/protocol/disco#info" {
            set node [wrapper::getattribute $child node]
            if {[string length $node] == 0} {
                lappend parts [list identity \
                                   [list name $Options(Name) \
                                        type text category gateway] 1 {} {}]
                lappend parts [list feature {var jabber:iq:version} 1 {} {}]
                lappend parts [list feature {var iq} 1 {} {}]
                lappend parts [list feature {var message} 1 {} {}]
                lappend parts [list feature {var "http://jabber.org/protocol/disco#info"} 1 {} {}]
                lappend parts [list feature {var "http://jabber.org/protocol/disco#items"} 1 {} {}]
                
                lappend qr [list query [list xmlns $query] 0 {} $parts]
                set rsp [list iq [list xmlns jabber:client type result id $id \
                                  to $requester from $self] 0 {} $qr]
            }
        }
        default {
            set rsp [RaiseIQ $query feature-not-implemented $id $self $requester]
        }
    }
    if {$rsp ne {}} {
        xmppd::jcp::route $Component [wrapper::createxml $rsp]
    }
    return
}

# chime::RaiseIQ --
#	
#	Raise an error response for invalid queries or for queries we do not intend
#	to handle.
#
proc ::chime::RaiseIQ {query type id self requester} {
    lappend p [list $type {xmlns urn:ietf:params:xml:ns:xmpp-stanzas} 1 {} {}]
    lappend qr [list query [list xmlns $query] 1 {} {}]
    lappend qr [list error {type cancel code 501} 0 {} $p]
    set ra [list xmlns jabber:client type error id $id \
        to $requester from $self]
    set rsp [list iq $ra 0 {} $qr]
}

# chime::presence --
#
#	Send a jabber presence message
#
proc ::chime::presence {from to type {show {online}} {status {}} {user {}}} {
    variable Component

    set kids {} ; set hist {}
    set ts [clock format [clock seconds] -format %Y%m%dT%H:%M:%S -gmt 1]
    lappend hist [list history [list maxchars 0 maxstanzas 0] 1 "" {}]
    lappend kids [list x {xmlns http://jabber.org/protocols/muc} 0 "" $hist]
    if {$show ne {}} {
        lappend kids [list show {} 0 $show {}]
    }
    if {$status ne {}} {
        lappend kids [list status {
            xmlns:xml http://www.w3.org/XML/1998/namespace
            xml:lang en-GB
        } 0 $status {}]
    }
    set attr [list from $from to $to xmlns jabber:client]
    if {$type ne {}} {lappend attr type $type}
    
    xmppd::jcp::route $Component \
        [wrapper::createxml [list presence $attr 0 "" $kids]]
    return
}

# chime::LoadConfig --
#
#	This procedure reads a text file and updates the Options array
#	from the contents. Comments and blank lines are ignored. All 
#	other lines must be a list of two elements, the first element 
#	must be an item in the Options array.
#
proc ::chime::LoadConfig {{conf {}}} {
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
        log warn "configuration file \"$conf\" could not be opened"
    }
    return
}

# chime::chimes --
#
#	Manage the scheduling of chimes on the hour.
#
proc ::chime::chimes {cmd} {
    variable ChimeId
    switch -exact -- $cmd {
        start {
            set ChimeId [after [nextchime] [namespace origin bong]]
        }
        stop {
            after cancel $ChimeId
        }
        default {
            return -code error "invalid option \"$cmd\": rtfm"
        }
    }
}

# chime::nextchime --
#
#	Calculate the number of milliseconds until the next hour.
#
proc ::chime::nextchime {} {
    set t [clock format [clock scan "+1 hour"] -format "%Y%m%d %H:00:00"]
    set delta [expr {([clock scan $t] - [clock seconds]) * 1000}]
    if {$delta < 60000} {
        puts stderr "error: chiming too fast"
        set delta 60000
    }
    puts "Schedule chime in $delta milliseconds"
    return $delta
}

# chime::bong --
#
#	Issue a timestamp message to the connected chatroom.
#
proc ::chime::bong {} {
    variable ChimeId
    variable Options
    variable Component

    after cancel $ChimeId
    set kids {}
    set ts [clock format [clock seconds] -format %Y%m%dT%H:%M:%S -gmt 1]
    puts "BONG at $ts"
    lappend kids [list body {} 0 \
                      [clock format [clock seconds] -gmt 1] {}]
    set from "$Options(Name)@$Options(JID)/$Options(Resource)"
    set attr [list from $from to $Options(Conference) \
                  type groupchat xmlns "jabber:client"]
    set xml [wrapper::createxml [list message $attr 0 "" $kids]]

    xmppd::jcp::route $Component $xml
    set ChimeId [after [nextchime] [namespace origin bong]]
}

proc ::chime::Main {} {
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
    set r [catch [linsert $argv 0 ::chime::Main] err]
    if {$r} {puts $errorInfo}
    exit $r
}
