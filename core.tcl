# core.tcl - Copyright (C) 2006 Pat Thoyts <patthoyts@users.sourceforge.net>
#
#  XMPP core utilities.
#
# RFC 3920 [http://www.ietf.org/rfc/rfc3920.txt]
# RFC 3921 [http://www.ietf.org/rfc/rfc3921.txt]
#
# -------------------------------------------------------------------------
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.
# -------------------------------------------------------------------------

package require xmppd::wrapper
package require logger;# tcllib 

namespace eval ::xmppd {
    
    variable version 0.1.0
    variable rcsid {$Id: core.tcl,v 1.4 2006/04/17 09:41:51 pat Exp $}

    namespace export configure cget xmlns jid Pop

    variable options
    if {![info exists options]} {
        array set options {
            domain    {}
            certfile  {}
            keyfile   {}
            modules   {}
            features  {}
            endpoints {}
            loglevel  warn
            logfile   {}
        }
    }

    variable xmlns
    if {![info exists xmlns]} {
        array set xmlns {
            client   jabber:client
            server   jabber:server
            dialback jabber:server:dialback
            stream   http://etherx.jabber.org/streams
            streams  urn:ietf:params:xml:ns:xmpp-streams
            sasl     urn:ietf:params:xml:ns:xmpp-sasl
            tls      urn:ietf:params:xml:ns:xmpp-tls
            bind     urn:ietf:params:xml:ns:xmpp-bind
            stanzas  urn:ietf:params:xml:ns:xmpp-stanzas
            session  urn:ietf:params:xml:ns:xmpp-session
            xml      http://www.w3.org/XML/1998/namespace
        }
    }

}

# -------------------------------------------------------------------------

proc ::xmppd::xmlns {name} {
    variable xmlns
    return $xmlns($name) 
}

proc ::xmppd::jid {part jid} {
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

proc ::xmppd::cget {option} {
    return [configure $option]
}

proc ::xmppd::configure {args} {
    variable options
    if {[llength $args] < 1} {
        set r {}
        foreach opt [lsort [array names options]] {
            lappend r -$opt $options($opt)
        }
        foreach module $options(modules) {
            set r [concat $r [${module}::_configure]]
        }
        return $r
    }

    set cget [expr {[llength $args] == 1 ? 1 : 0}]
    while {[string match -* [set option [lindex $args 0]]]} {
        switch -glob -- $option {
            -domain {
                if {$cget} {
                    return $options(domain)
                } else {
                    set options(domain) [Pop args 1]
                }
            }
            -loglevel {
                if {$cget} {
                    return $options(loglevel)
                } else {
                    variable log
                    set options(loglevel) [Pop args 1]
                    if {![info exists log]} {
                        LogInit xmppd $options(loglevel)
                    } else {
                        ${log}::setlevel $options(loglevel)
                    }
                }
            }
            -logfile {
                if {$cget} {
                    return $options(logfile)
                } else {
                    set options(logfile) [Pop args 1]
                    LogSetFile $options(logfile)
                }
            }
            -certfile {
                if {$cget} {
                    return $options(certfile)
                } else {
                    set options(certfile) [Pop args 1]
                }
            }
            -keyfile {
                if {$cget} {
                    return $options(keyfile)
                } else {
                    set options(keyfile) [Pop args 1]
                }
            }
            -features {
                if {$cget} { return $options(features) }
            }
            -modules {
                if {$cget} { return $options(modules) }
            }
            -- { Pop args ; break }
            default {
                if {$cget} {
                    foreach module $options(modules) {
                        if {![catch {${module}::_configure $option} r]} {
                            return $r
                        }
                    }
                    return -code error "bad option \"$option\""
                } else {
                    set value [Pop args 1]
                    set r 1
                    foreach module $options(modules) {
                        set r [catch {${module}::_configure $option $value} res]
                        if {! $r} { break }
                    }
                    if {$r} {
                        return -code error "bad option \"$option\""
                    }
                }
            }
        }
        Pop args
    }
    return
}

proc ::xmppd::register {type args} {
    variable options
    switch -exact -- $type {
        module {
            foreach module $args {
                if {[lsearch -exact $options(modules) $module] == -1} {
                    lappend options(modules) $module
                }
            }
        }
        
        feature {
            foreach {name uri} $args {
                if {[string length $name] < 1} { return -code error "must provide a name" }
                if {[string length $uri] < 1} {return -code error "must provide a value" }
                array set f $options(features)
                set f($name) $uri
                set options(features) [array get f]
            }
        }

        default {
            return -code error "invalid type \"$type\": must be one of\
                module or feature"
        }
    }
}

proc ::xmppd::route {from to xml} {
    set domain [jid domain $to]
    if {$domain eq [cget -domain]} {
        xmppd::s2c::route $from $to $xml
    } else {
        xmppd::s2s::route -from $from -to $to $xml
    }
}

# -------------------------------------------------------------------------
# Logging functions

proc ::xmppd::LogInit {service level} {
    variable log
    set log [logger::init $service]
    ${log}::setlevel $level
    proc ${log}::stdoutcmd {level text} {
        variable service
        variable logfile
        set ts [clock format [clock seconds] -format {%H:%M:%S}]
        if {[::info exists logfile] && $logfile ne ""} {
            puts $logfile "\[$ts\] $level $text"
        }
        puts stderr $text
    }
}

proc ::xmppd::LogSetFile {filename} {
    variable log
    if {[string length $filename] > 0} {
        set code {
            variable logfile
            if {[::info exists logfile]} { ::catch {::close $logfile} }
            set logfile [::open %FILE a+]
            fconfigure $logfile -buffering line
            puts $logfile [clock format [clock seconds] \
                -format "---- %Y%m%dT%H:%M:%S [string repeat - 49]"]
        }
        namespace eval $log [string map [list %FILE $filename] $code]
    }
}

proc ::xmppd::Log {component level msg} {
    variable log
    ${log}::${level} "$component: $msg"
}

# -------------------------------------------------------------------------
# utility stuff

#  Pop the nth element off a list. Used in options processing.
#
proc ::xmppd::Pop {varname {nth 0}} {
    upvar $varname args
    set r [lindex $args $nth]
    set args [lreplace $args $nth $nth]
    return $r
}


# -------------------------------------------------------------------------

namespace eval ::xmppd {
    if {![info exists log]} {
        LogInit xmppd $options(loglevel)
    }
}

package provide xmppd::core $::xmppd::version

# -------------------------------------------------------------------------
# Local variables:
#   mode: tcl
#   indent-tabs-mode: nil
# End:

