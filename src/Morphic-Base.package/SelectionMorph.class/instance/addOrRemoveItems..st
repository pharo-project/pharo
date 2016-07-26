halo commands
addOrRemoveItems: handOrEvent 
	"Make a new selection extending the current one."

	| hand |
	hand := (handOrEvent isMorphicEvent) 
				ifFalse: [handOrEvent]
				ifTrue: [handOrEvent hand].
	hand 
		addMorphBack: ((self class 
				newBounds: (hand lastEvent cursorPoint extent: 16 @ 16)) 
					setOtherSelection: self).
