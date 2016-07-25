keyboard
leftArrow
	
	(menuMorph hideDetail)	
			ifFalse: [ 
				self closeMenu.
				^ false ].	
	^ true