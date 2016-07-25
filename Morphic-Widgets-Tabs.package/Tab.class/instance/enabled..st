protocol
enabled: aBoolean

	(state isEnabledMatchingTheState: aBoolean) ifTrue: [ ^ self ].
	
	state := state newStateForEnabled: aBoolean.
	label enabled: aBoolean.
	self changed