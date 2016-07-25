control
activeSubmenu: aSubmenu 
	activeSubMenu ifNotNil: [activeSubMenu delete].
	activeSubMenu := aSubmenu.
	aSubmenu isNil ifTrue: [^ self].
	activeSubMenu selectItem: nil event: nil.
	activeSubMenu borderColor: self borderColor.
	activeSubMenu beSticky.
	activeSubMenu resistsRemoval: true.
	activeSubMenu removeMatchString.