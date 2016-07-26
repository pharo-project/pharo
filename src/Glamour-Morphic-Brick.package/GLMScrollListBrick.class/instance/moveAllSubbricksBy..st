layouting
moveAllSubbricksBy: anInteger

	self band subbricks do: [ :each | each translateY: anInteger ].
	self band translateY: anInteger negated