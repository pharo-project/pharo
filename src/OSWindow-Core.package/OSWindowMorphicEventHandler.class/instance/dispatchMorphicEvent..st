events
dispatchMorphicEvent: anEvent
	morphicWorld defer: [
		(morphicWorld activeHand isNotNil and: [ anEvent hand isNotNil ]) ifTrue: [
			morphicWorld activeHand handleEvent: anEvent
		]
	].
	
	