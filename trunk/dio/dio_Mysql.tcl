# dio_Mysql.tcl -- Mysql backend.

# Copyright 2002-2004 The Apache Software Foundation

# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at

#	http://www.apache.org/licenses/LICENSE-2.0

# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

# $Id: dio_Mysql.tcl 406282 2006-05-14 08:46:50Z davidw $

package provide dio_Mysql 0.1

namespace eval DIO {
    ::itcl::class Mysql {
	inherit Database

	constructor {args} {eval configure $args} {
	    if {       [catch {package require Mysqltcl}]   \
	    	    && [catch {package require mysqltcl}]   \
		    && [catch {package require mysql}   ] } {
		return -code error "No MySQL Tcl package available"
	    }

	    eval configure $args

	    if {[lempty $db]} {
		if {[lempty $user]} {
		    set user $::env(USER)
		}
		set db $user
	    }
	}

	destructor {
	    close
	}

	method open {} {
	    set command "mysqlconnect"

	    if {![lempty $user]} { lappend command -user $user }
	    if {![lempty $pass]} { lappend command -password $pass }
	    if {![lempty $port]} { lappend command -port $port }
	    if {![lempty $host]} { lappend command -host $host }

	    if {[catch $command error]} { return -code error $error }

	    set conn $error

	    if {![lempty $db]} { mysqluse $conn $db }
	}

	method close {} {
	    if {![info exists conn]} { return }
	    catch {mysqlclose $conn}
	    unset conn
	}

	method exec {req} {
	    if {![info exists conn]} { open }

	    set cmd mysqlexec
            set sqlcmd [::string tolower [::string range $req 0 5]]
	    if {[::string compare $sqlcmd "select"] == 0} { set cmd mysqlsel }

	    set errorinfo ""
            set req [::string map [::list "\\" "\\\\"] $req]
	    if {[catch {$cmd $conn $req} error]} {
		set errorinfo $error
		set obj [result Mysql -resultid [::list $conn] \
                             -errorcode 1 -errorinfo [::list $error]]
		return $obj
	    }
	    if {[catch {mysqlcol $conn -current name} fields]} { set fields "" }
	    set obj [result Mysql -resultid [::list $conn] \
			 -numrows [::list $error] -fields [::list $fields]]
            if {[$obj error]} { set errorinfo [$obj errorinfo] }
	    return $obj
	}

	method lastkey {} {
	    if {![info exists conn]} { return }
	    return [mysqlinsertid $conn]
	}

	method quote {string} {
	    if {![catch {mysqlescape $string} result]} { return $result }
	    return [string map {"'" "''"} $string]
	}

	method sql_limit_syntax {limit {offset ""}} {
	    if {[lempty $offset]} {
		return " LIMIT $limit"
	    }
	    return " LIMIT [expr $offset - 1],$limit"
	}

	method handle {} {
	    if {![info exists conn]} { open }

	    return $conn
	}

	method makeDBFieldValue {table_name field_name val {convert_to {}}} {
		if {[info exists specialFields(${table_name}@${field_name})]} {
		    switch $specialFields(${table_name}@${field_name}) {
			DATE {
			  	set secs [clock scan $val]
				set my_val [clock format $secs -format {%Y-%m-%d}]
				return "DATE_FORMAT('$my_val', '%Y-%m-%d')"
			  }
			DATETIME {
			  	set secs [clock scan $val]
				set my_val [clock format $secs -format {%Y-%m-%d %T}]
				return "DATE_FORMAT('$my_val', '%Y-%m-%d %T')"
			  }
			NOW {
			    switch $convert_to {
				SECS {
				    if {[::string compare $val "now"] == 0} {
					set	secs    [clock seconds]
					set	my_val  [clock format $secs -format {%Y%m%d%H%M%S}]
					return	$my_val
				    } else {
					return  "DATE_FORMAT(session_update_time,'%Y%m%d%H%i%S')"
				    }
				}
				default {
				    if {[::string compare $val, "now"] == 0} {
			  		set secs [clock seconds]
				    } else {
					set secs [clock scan $val]
				    }
				    set my_val [clock format $secs -format {%Y-%m-%d %T}]
				    return "DATE_FORMAT('$my_val', '%Y-%m-%d %T')"
				}
			    }
			}
			default {
			  	# no special code for that type!!
				return "'[quote $val]'"
			}
		    }
		} else {
			return "'[quote $val]'"
		}
	}

	public variable db "" {
	    if {[info exists conn]} {
		mysqluse $conn $db
	    }
	}

	public variable interface	"Mysql"
	private variable conn

    } ; ## ::itcl::class Mysql

    ::itcl::class MysqlResult {
	inherit Result

	constructor {args} {
	    eval configure $args
            if {"$resultid" == ""} {
                return -code error "no valid result id present"
            }
	}

	destructor {

	}

	method nextrow {} {
	    return [mysqlnext $resultid]
	}
	
    } ; ## ::itcl::class MysqlResult

}
