menu
addModelYellowButtonItemsTo: aCustomMenu event: evt 
	"Give my models a chance to add their context-menu items to  
	aCustomMenu."
	self model
		ifNotNil: [:mod |
			mod
				addModelYellowButtonMenuItemsTo: aCustomMenu
				forMorph: self
				hand: evt hand]