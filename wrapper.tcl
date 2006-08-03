################################################################################
#
# wrapper.tcl 
#
# This file defines wrapper procedures.  These
# procedures are called by functions in jabberlib, and
# they in turn call the TclXML library functions.
#
# Seems to be originally written by Kerem HADIMLI, with additions
# from Todd Bradley. Completely rewritten from scratch by Mats Bengtsson.
# The algorithm for building parse trees has been completely redesigned.
# Only some structures and API names are kept essentially unchanged.
#
# $Id: wrapper.tcl,v 1.2 2004/12/09 09:12:55 pat Exp $
# 
# ########################### INTERNALS ########################################
# 
# The whole parse tree is stored as a hierarchy of lists as:
# 
#       parent = {tag attrlist isempty cdata {child1 child2 ...}}
#       
# where the childs are in turn a list of identical structure:      
# 
#       child1 = {tag attrlist isempty cdata {grandchild1 grandchild2 ...}}
#       child2 = {tag attrlist isempty cdata {grandchild1 grandchild2 ...}}
#       
# etc.
#       
# ########################### USAGE ############################################
#
#   NAME
#      wrapper::new - a wrapper for the TclXML parser.
#   SYNOPSIS
#      wrapper::new streamstartcmd streamendcmd parsecmd errorcmd
#   OPTIONS
#      none
#   COMMANDS
#      wrapper::reset wrapID
#      wrapper::createxml xmllist
#      wrapper::createtag tagname ?args?
#      wrapper::getattr attrlist attrname
#      wrapper::setattr attrlist attrname value
#      wrapper::parse id xml
#      wrapper::xmlcrypt chdata
#      wrapper::gettag xmllist
#      wrapper::getattrlist xmllist
#      wrapper::getisempty xmllist
#      wrapper::getcdata xmllist
#      wrapper::getchildren xmllist
#      wrapper::getattribute xmllist attrname
#      wrapper::setattrlist xmllist attrlist
#      wrapper::setcdata xmllist cdata
#      wrapper::splitxml xmllist tagVar attrVar cdataVar childVar
#
# ########################### LIMITATIONS ######################################
# 
# Mixed elements of character data and elements are not working.
# 
# ########################### CHANGES ##########################################
#
#       0.*      by Kerem HADIMLI and Todd Bradley
#       1.0a1    complete rewrite, and first release by Mats Bengtsson
#       1.0a2    a few fixes
#       1.0a3    wrapper::reset was not right, -ignorewhitespace, 
#                -defaultexpandinternalentities
#       1.0b1    added wrapper::parse command, configured for expat, 
#                return break at stream end
#       1.0b2    fix to make parser reentrant
#       030910   added accessor functions to get/set xmllist elements
#       031103   added splitxml command

package require tdom 0.8

namespace eval wrapper {

    # The public interface.
    namespace export what

    # Keep all internal data in this array, with 'id' as first index.
    variable wrapper
    variable debug 1
    
    # Running id that is never reused; start from 0.
    set wrapper(freeid) 0
    
    # Keep all 'id's in this list.
    set wrapper(list) {}
    
    variable xmldefaults {-isempty 1 -attrlist {} -chdata {} -subtags {}}
}

# wrapper::new --
#
#       Contains initializations needed for the wrapper.
#       Sets up callbacks via the XML parser.
#       
# Arguments:
#       streamstartcmd:   callback when level one start tag received
#       streamendcmd:     callback when level one end tag received
#       parsecmd:         callback when level two end tag received
#       errorcmd          callback when receiving an error from the XML parser.
#                         Must all be fully qualified names.
#       
# Results:
#       A unique wrapper id.

proc wrapper::new {streamstartcmd streamendcmd parsecmd errorcmd args} {
    variable wrapper
    variable debug
    
    if {$debug > 1}  {
	puts "wrapper::new"
    }
    
    set parseropt "-namespace"
    while {[string match -* [set option [lindex $args 0]]]} {
        switch -exact -- $option {
            -namespace {
                if {[lindex $args 1] == 0} {
                    set parseropt ""
                }
                set args [lrange $args 1 end] 
            }
        }
        set args [lrange $args 1 end]
    }

    # Handle id of the wrapper.
    set id "wrap$wrapper(freeid)"
    incr wrapper(freeid)
    lappend wrapper(list) $id
    
    set wrapper($id,streamstartcmd) $streamstartcmd
    set wrapper($id,streamendcmd) $streamendcmd
    set wrapper($id,parsecmd) $parsecmd
    set wrapper($id,errorcmd) $errorcmd
    
    # Create the actual XML parser. It is created in our present namespace,
    # at least for the tcl parser!!!
    set wrapper($id,parser) [eval [linsert $parseropt 0 xml::parser]]
    set wrapper($id,class) "expat"
    $wrapper($id,parser) configure   \
      -final 0    \
      -elementstartcommand  [list [namespace current]::elementstart $id]   \
      -elementendcommand    [list [namespace current]::elementend $id]     \
      -characterdatacommand [list [namespace current]::chdata $id]         \
      -ignorewhitespace     1
    
    puts "parser namespace handling: [$wrapper($id,parser) cget -namespace]"

    # Current level; 0 before root tag; 1 just after root tag, 2 after 
    # command tag, etc.
    set wrapper($id,level) 0
    set wrapper($id,levelonetag) {}
    
    # Level 1 is the main tag, <stream:stream>, and level 2
    # is the command tag, such as <message>. We don't handle level 1 xmldata.
    set wrapper($id,tree,2) {}        
    return $id
}

# wrapper::parse --
#
#       For parsing xml. 
#       
# Arguments:
#       id:          the wrapper id.
#       xml:         raw xml data to be parsed.
#       
# Results:
#       none.

proc wrapper::parse {id xml} {
    variable wrapper

    # This is not as innocent as it looks; the 'tcl' parser proc is created in
    # the creators namespace (wrapper::), but the 'expat' parser ???
    set parser $wrapper($id,parser)
    parsereentrant $parser $xml
    return {}
}

# Reentrant xml parser wrapper. This ought to go in the parser!

namespace eval wrapper {

    # A reference counter for reentries.
    variable refcount 0
    
    # Stack for xml.
    variable stack ""
}

# wrapper::parsereentrant --
# 
#       Forces parsing to be serialized in an event driven environment.
#       If we read xml from socket and happen to trigger a read (and parse)
#       event right from an element callback, everyhting will be out of sync.
#       
# Arguments:
#       p:           the parser.
#       xml:         raw xml data to be parsed.
#       
# Results:
#       none.

proc wrapper::parsereentrant {p xml} {
    variable refcount
    variable stack
    
    incr refcount
    if {$refcount == 1} {
	
	# This is the main entry: do parse original xml.
	$p parse $xml
	
	# Parse everything on the stack (until empty?).
	while {[string length $stack] > 0} {
	    set tmpstack $stack
	    set stack ""
	    $p parse $tmpstack
	}
    } else {
	
	# Reentry, put on stack for delayed execution.
	append stack $xml
    }
    incr refcount -1
    return {}
}

# wrapper::elementstart --
#
#       Callback proc for all element start.
#       
# Arguments:
#       id:          the wrapper id.
#       tagname:     the element (tag) name.
#       attrlist:    list of attributes {key value key value ...}
#       args:        additional arguments given by the parser.
#       
# Results:
#       none.

proc wrapper::elementstart {id tagname attrlist args} {
    variable wrapper
    variable debug
    
    if {$debug > 1}  {
	puts "wrapper::elementstart id=$id, tagname=$tagname,  \
	  attrlist='$attrlist', args=$args"
    }
    
    # Check args, to see if empty element and/or namespace. 
    # Put xmlns in attribute list.
    array set argsarr $args
    set isempty 0
    if {[info exists argsarr(-empty)]} {
	set isempty $argsarr(-empty)
    }
    if {[info exists argsarr(-namespacedecls)]} {
	lappend attrlist xmlns [lindex $argsarr(-namespacedecls) 0]
    }
    
    if {[set ndx [string last : $tagname]] != -1} {
        set ns [string range $tagname 0 [expr {$ndx - 1}]]
        set tagname [string range $tagname [incr ndx] end]
        lappend attrlist xmlns $ns
        if {$debug > 1} {
            puts "        exploded [list $ns $tagname]"
        }
    }

    if {$wrapper($id,level) == 0} {
	
	# We got a root tag, such as <stream:stream>
	set wrapper($id,level) 1
	set wrapper($id,levelonetag) $tagname 
	set wrapper($id,tree,1) [list $tagname $attrlist $isempty {} {}]
	
	# Do the registered callback at the global level.
	uplevel #0 $wrapper($id,streamstartcmd) $attrlist
	
    } else {
	
	# This is either a level 2 command tag, such as 'presence', 'iq', or 'message',
	# or we have got a new tag beyond level 2.
	# It is time to start building the parse tree.
	set level [incr wrapper($id,level)]
	set wrapper($id,tree,$level) [list $tagname $attrlist $isempty {} {}]
    }
}

# wrapper::elementend --
#
#       Callback proc for all element ends.
#       
# Arguments:
#       id:          the wrapper id.
#       tagname:     the element (tag) name.
#       args:        additional arguments given by the parser.
#       
# Results:
#       none.

proc wrapper::elementend {id tagname args} {
    variable wrapper
    variable debug
    
    if {$debug > 1}  {
	puts "wrapper::elementend id=$id, tagname=$tagname,  \
	  args='$args', level=$wrapper($id,level)"
    }
    
    # Check args, to see if empty element
    set isempty 0
    set ind [lsearch $args {-empty}]
    if {$ind >= 0} {
	set isempty [lindex $args [expr {$ind + 1}]]
    }
    if {$wrapper($id,level) == 1} {
	
	# End of the root tag (</stream:stream>).
	# Do the registered callback at the global level.
	uplevel #0 $wrapper($id,streamendcmd)
	
	incr wrapper($id,level) -1
	
	# We are in the middle of parsing, need to break.
	reset $id
	return -code 3
    } else {
	
	# We are finshed with this child tree.
	set childlevel $wrapper($id,level)
	
	# Insert the child tree in the parent tree.
	set level [incr wrapper($id,level) -1]
	append_child $id $level $wrapper($id,tree,$childlevel)

	if {$level == 1} {
	    
	    # We've got an end tag of a command tag, and it's time to
	    # deliver our parse tree to the registered callback proc.
	    uplevel #0 "$wrapper($id,parsecmd) [list $wrapper($id,tree,2)]"
	}
    }
}

# wrapper::append_child --
#
#       Inserts a child element data in level temp data.
#       
# Arguments:
#       id:          the wrapper id.
#       level:       the parent level, child is level+1.
#       childtree:   the tree to append.
#       
# Results:
#       none.

proc wrapper::append_child {id level childtree} {
    variable wrapper
    variable debug

    if {$debug > 1} {
	puts "wrapper::append_child id=$id, level=$level, childtree='$childtree'"
    }

    # Get child list at parent level (level).
    set childlist [lindex $wrapper($id,tree,$level) 4]
    lappend childlist $childtree
    
    # Build the new parent tree.
    set wrapper($id,tree,$level) [lreplace $wrapper($id,tree,$level) 4 4  \
      $childlist]
}

# wrapper::chdata --
#
#       Appends character data to the tree level xml chdata.
#       It makes also internal entity replacements on character data.
#       Callback from the XML parser.
#       
# Arguments:
#       id:          the wrapper id.
#       chardata:    the character data.
#       
# Results:
#       none.

proc wrapper::chdata {id chardata} {   
    variable wrapper
    variable debug

    if {$debug > 2}  {
	puts "wrapper::chdata id=$id, chardata='$chardata',  \
	  level=$wrapper($id,level)"
    }
    set level $wrapper($id,level)
    
    # If we receive CHDATA before any root element, 
    # or after the last root element, discard.
    if {$level <= 0} {
	return
    }
    set chdata [lindex $wrapper($id,tree,$level) 3]
    
    # Make standard entity replacements.
    append chdata [xmldecrypt $chardata]
    set wrapper($id,tree,$level)    \
      [lreplace $wrapper($id,tree,$level) 3 3 "$chdata"]
}

# wrapper::reset --
#
#       Resets the wrapper and XML parser to be prepared for a fresh new 
#       document.
#       If done while parsing be sure to return a break (3) from callback.
#       
# Arguments:
#       id:          the wrapper id.
#       
# Results:
#       none.

proc wrapper::reset {id} {   
    variable wrapper
    variable debug

    if {$debug > 1} {
	puts "wrapper::reset id=$id"
    }

    if {0} {
	
	# This resets the actual XML parser. Not sure this is actually needed.
	$wrapper($id,parser) reset
	if {$debug > 1} {
	    puts "   wrapper::reset configure parser"
	}
    
	$wrapper($id,parser) configure   \
	    -final 0    \
	    -elementstartcommand  [list [namespace current]::elementstart $id]   \
	    -elementendcommand    [list [namespace current]::elementend $id]     \
	    -characterdatacommand [list [namespace current]::chdata $id]         \
	    -ignorewhitespace     1

    }
    
    # Cleanup internal state vars.
    set lev 2
    while {[info exists wrapper($id,tree,$lev)]} {
	unset wrapper($id,tree,$lev)
	incr lev
    }
    
    # Reset also our internal wrapper to its initial position.
    set wrapper($id,level) 0
    set wrapper($id,levelonetag) {}
    set wrapper($id,tree,2) {}  
}

# wrapper::xmlerror --
#
#       Callback from the XML parser when error received. Resets wrapper,
#       and makes a 'streamend' command callback.
#
# Arguments:
#       id:          the wrapper id.
#       
# Results:
#       none.

proc wrapper::xmlerror {id args} {
    variable wrapper
    variable debug

    if {$debug > 1} {
	puts "wrapper::xmlerror id=$id, args='$args'"
    }

    # Resets the wrapper and XML parser to be prepared for a fresh new document.
    #reset $id
    #uplevel #0 $wrapper($id,errorcmd) [list $args] ????
    uplevel #0 $wrapper($id,errorcmd) $args
    #reset $id
    return -code error {Fatal XML error}
}

# wrapper::createxml --
#
#       Creates raw xml data from a hierarchical list of xml code.
#       This proc gets called recursively for each child.
#       It makes also internal entity replacements on character data.
#       Mixed elements aren't treated correctly generally.
#       
# Arguments:
#       xmllist     a list of xml code in the format described in the header.
#       
# Results:
#       raw xml data.

proc wrapper::createxml {xmllist} {
        
    # Extract the XML data items.
    foreach {tag attrlist isempty chdata childlist} $xmllist {break}
    set rawxml "<$tag"
    foreach {attr value} $attrlist {
	append rawxml " ${attr}='${value}'"
    }
    if {$isempty} {
	append rawxml "/>"
    } else {
	append rawxml ">"
	
	# Call ourselves recursively for each child element. 
	# There is an arbitrary choice here where childs are put before PCDATA.
	foreach child $childlist {
	    append rawxml [createxml $child]
	}
	
	# Make standard entity replacements.
	if {[string length $chdata]} {
	    append rawxml [xmlcrypt $chdata]
	}
	append rawxml "</$tag>"
    }
    return $rawxml
}

# wrapper::createtag --
#
#       Build an element list given the tag and the args.
#
# Arguments:
#       tagname:    the name of this element.
#       args:       
#           -empty   0|1      Is this an empty tag? If $chdata 
#                             and $subtags are empty, then whether 
#                             to make the tag empty or not is decided 
#                             here. (default: 1)
#	    -attrlist {attr1 value1 attr2 value2 ..}   Vars is a list 
#                             consisting of attr/value pairs, as shown.
#	    -chdata $chdata   ChData of tag (default: "").
#	    -subtags {$subchilds $subchilds ...} is a list containing xmldata
#                             of $tagname's subtags. (default: no sub-tags)
#       
# Results:
#       a list suitable for wrapper::createxml.

proc wrapper::createtag {tagname args} {
    variable xmldefaults
    
    # Fill in the defaults.
    array set xmlarr $xmldefaults
    
    # Override the defults with actual values.
    if {[llength $args] > 0} {
	array set xmlarr $args
    }
    if {!(($xmlarr(-chdata) == "") && ($xmlarr(-subtags) == ""))} {
	set xmlarr(-isempty) 0
    }
    
    # Build sub elements list.
    set sublist {}
    foreach child $xmlarr(-subtags) {
	lappend sublist $child
    }
    set xmllist [list $tagname $xmlarr(-attrlist) $xmlarr(-isempty)  \
      $xmlarr(-chdata) $sublist]
    return $xmllist
}

# wrapper::getattr --
#
#       This proc returns the value of 'attrname' from 'attrlist'.
#
# Arguments:
#       attrlist:   a list of key value pairs for the attributes.
#       attrname:   the name of the attribute which value we query.
#       
# Results:
#       value of the attribute or empty.

proc wrapper::getattr {attrlist attrname} {

    foreach {attr val} $attrlist {
	if {[string equal $attr $attrname]} {
	    return $val
	}
    }
    return {}
}

proc wrapper::getattribute {xmllist attrname} {

    foreach {attr val} [lindex $xmllist 1] {
	if {[string equal $attr $attrname]} {
	    return $val
	}
    }
    return {}
}

proc wrapper::isattr {attrlist attrname} {

    foreach {attr val} $attrlist {
	if {[string equal $attr $attrname]} {
	    return 1
	}
    }
    return 0
}

proc wrapper::isattribute {xmllist attrname} {

    foreach {attr val} [lindex $xmllist 1] {
	if {[string equal $attr $attrname]} {
	    return 1
	}
    }
    return 0
}

proc wrapper::setattr {attrlist attrname value} {

    array set attrArr $attrlist
    set attrArr($attrname) $value
    return [array get attrArr]
}

# wrapper::gettag, getattrlist, getisempty, ,getcdata, getchildren  --
#
#       Accessor functions for 'xmllist'.
#       {tag attrlist isempty cdata {grandchild1 grandchild2 ...}}
#
# Arguments:
#       xmllist:    an xml hierarchical list.
#       
# Results:
#       list of childrens if any.

proc wrapper::gettag {xmllist} {
    return [lindex $xmllist 0]
}

proc wrapper::getattrlist {xmllist} {
    return [lindex $xmllist 1]
}

proc wrapper::getisempty {xmllist} {
    return [lindex $xmllist 2]
}

proc wrapper::getcdata {xmllist} {
    return [lindex $xmllist 3]
}

proc wrapper::getchildren {xmllist} {
    return [lindex $xmllist 4]
}

proc wrapper::splitxml {xmllist tagVar attrVar cdataVar childVar} {
    
    foreach {tag attr empty cdata children} $xmllist break
    uplevel 1 [list set $tagVar $tag]
    uplevel 1 [list set $attrVar $attr]
    uplevel 1 [list set $cdataVar $cdata]
    uplevel 1 [list set $childVar $children]    
}

proc wrapper::getchildswithtag {xmllist tag} {
    
    set clist {}
    foreach celem [lindex $xmllist 4] {
	if {[string equal [lindex $celem 0] $tag]} {
	    lappend clist $celem
	}
    }
    return $clist
}

proc wrapper::getchildwithtaginnamespace {xmllist tag ns} {
    
    set clist {}
    foreach celem [lindex $xmllist 4] {
	if {[string equal [lindex $celem 0] $tag]} {
	    unset -nocomplain attrArr
	    array set attrArr [lindex $celem 1]
	    if {[info exists attrArr(xmlns)] &&  \
	      [string equal $attrArr(xmlns) $ns]} {
		lappend clist $celem
		break
	    }
	}
    }
    return $clist
}

proc wrapper::getfromchilds {childs tag} {
    
    set clist {}
    foreach celem $childs {
	if {[string equal [lindex $celem 0] $tag]} {
	    lappend clist $celem
	}
    }
    return $clist
}

proc wrapper::getnamespacefromchilds {childs tag ns} {
    
    set clist {}
    foreach celem $childs {
	if {[string equal [lindex $celem 0] $tag]} {
	    unset -nocomplain attrArr
	    array set attrArr [lindex $celem 1]
	    if {[info exists attrArr(xmlns)] &&  \
	      [string equal $attrArr(xmlns) $ns]} {
		lappend clist $celem
		break
	    }
	}
    }
    return $clist
}

proc wrapper::setattrlist {xmllist attrlist} {
 
    return [lreplace $xmllist 1 1 $attrlist]
}

proc wrapper::setcdata {xmllist cdata} {
 
    return [lreplace $xmllist 3 3 $cdata]
}

proc wrapper::setchildlist {xmllist childlist} {

    return [lreplace $xmllist 4 4 $childlist]
}

# wrapper::xmlcrypt --
#
#       Makes standard XML entity replacements.
#
# Arguments:
#       chdata:     character data.
#       
# Results:
#       chdata with XML standard entities replaced.

proc wrapper::xmlcrypt {chdata} {

    foreach from {\& < > {"} {'}}   \
      to {{\&amp;} {\&lt;} {\&gt;} {\&quot;} {\&apos;}} {
	regsub -all $from $chdata $to chdata
    }	
    return $chdata
}

# wrapper::xmldecrypt --
#
#       Replaces the XML standard entities with real characters.
#
# Arguments:
#       chdata:     character data.
#       
# Results:
#       chdata without any XML standard entities.

proc wrapper::xmldecrypt {chdata} {

    foreach from {{\&amp;} {\&lt;} {\&gt;} {\&quot;} {\&apos;}}   \
      to {{\&} < > {"} {'}} {
	regsub -all $from $chdata $to chdata
    }	
    return $chdata
}

# wrapper::parse_xmllist_to_array --
#
#       Takes a hierarchical list of xml data and parses the character data
#       into array elements. The array key of each element is constructed as:
#       rootTag_subTag_subSubTag.
#       Repetitative elements are not parsed correctly.
#       Mixed elements of chdata and tags are not allowed.
#       This is typically called without a 'key' argument.
#
# Arguments:
#       xmllist:    a hierarchical list of xml data as defined above.
#       arrName:
#       key:        (optional) the rootTag, typically only used internally.
#       
# Results:
#       none. Array elements filled.

proc wrapper::parse_xmllist_to_array {xmllist arrName {key {}}} {

    upvar #0 $arrName locArr
    
    # Return if empty element.
    if {[lindex $xmllist 2]} {
	return
    }
    if {[string length $key]} {
	set und {_}
    } else {
	set und {}
    }
    
    set childs [lindex $xmllist 4]
    if {[llength $childs]} {
	foreach c $childs {
	    set newkey "${key}${und}[lindex $c 0]"
	    
	    # Call ourselves recursively.
	    parse_xmllist_to_array $c $arrName $newkey
	}
    } else {
	
	# This is a leaf of the tree structure.
	set locArr($key) [lindex $xmllist 3]
    }
    return {}
}

#-------------------------------------------------------------------------------

package provide xmppd::wrapper 1.0.0

# -------------------------------------------------------------------------

