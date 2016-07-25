hal-algorithm
markCycleWith: anInteger

	self cycleNodes do: [ :next | next hNumber: anInteger ].
	^ self cycleNodes 