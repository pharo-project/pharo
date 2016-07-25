instance creation
withPassenger: anObject from: source hand: dragHand
	| hand |
	
	hand := (dragHand ifNil:[ ActiveHand ]).
	
	^ self new
		passenger: anObject;
		source: source;
		"If the client hasn't provided a hand use the currently active hand"
		dragHand: hand;
		shouldCopy: hand shiftPressed;
		position: source positionInWorld;
		yourself