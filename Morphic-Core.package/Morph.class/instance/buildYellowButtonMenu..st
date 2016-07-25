menu
buildYellowButtonMenu: aHand 
	"build the morph menu for the yellow button"
	| menu |
	menu := UIManager default newMenuIn: self for: self.
	self addNestedYellowButtonItemsTo: menu event: ActiveEvent.
	^ menu