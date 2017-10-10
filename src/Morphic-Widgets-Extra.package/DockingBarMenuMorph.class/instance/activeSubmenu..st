control
activeSubmenu: aSubmenu 
	super activeSubmenu: aSubmenu.
	aSubmenu
		ifNotNil: [activeSubMenu activatedFromDockingBar: nil]