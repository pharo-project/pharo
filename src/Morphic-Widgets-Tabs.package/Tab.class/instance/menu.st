accessing
menu
	| newMenu |
	
	newMenu := MenuMorph new defaultTarget: self.
	menu ifNotNil: [ menu cull: newMenu ].
	
	self addRefreshMenuItemOn: newMenu.
	
	^ newMenu