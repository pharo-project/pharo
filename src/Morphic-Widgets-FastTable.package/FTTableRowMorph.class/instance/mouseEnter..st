event handling
mouseEnter: evt

	self color: (
		selectionColor isTransparent 
			ifTrue: [ self mouseOverColor ]
			ifFalse: [ self mouseOverColor mixed: 0.5 with: selectionColor ]
	)