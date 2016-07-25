event handling
mouseDown: anEvent
	| selectors |
	selectors := Array 
		with: #click:
		with: nil
		with: nil
		with: (self dragEnabled ifTrue:[#startDrag:] ifFalse:[nil]).
	anEvent hand waitForClicksOrDrag: self event: anEvent selectors: selectors threshold: 10.
	super mouseDown: anEvent