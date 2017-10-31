adding
submenuFor: aMenu

	| submenu |
	
	submenu := MenuMorph new.
	submenu target: self.
	
	self addRubricItemsIn: submenu.
	


	aMenu 
	add: 'Transcript' 
	icon: (Smalltalk ui icons iconNamed: #transcriptIcon) 
	subMenu: submenu.