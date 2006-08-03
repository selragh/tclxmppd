# pkgIndex.tcl - Copyright (C) 2004 Pat Thoyts <patthoyts@users.sf.net>
#
# Declare tclxmppd packages.
#
# $Id: pkgIndex.tcl,v 1.1 2004/11/28 10:20:34 pat Exp $

package ifneeded xmppd::core 1.0.0 [list source [file join $dir core.tcl]]
package ifneeded xmppd::s2s  1.0.0 [list source [file join $dir s2s.tcl]]
package ifneeded xmppd::s2c  1.0.0 [list source [file join $dir s2c.tcl]]
package ifneeded xmppd::sm   1.0.0 [list source [file join $dir sm.tcl]]
package ifneeded xmppd::jcp  1.0.0 [list source [file join $dir jcp.tcl]]
package ifneeded xmppd::wrapper 1.0.0 [list source [file join $dir wrapper.tcl]]
