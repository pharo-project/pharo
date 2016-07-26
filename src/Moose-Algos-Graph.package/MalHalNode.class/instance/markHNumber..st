hal-algorithm
markHNumber: anInteger

	self hNumber: anInteger.
	self isInCycle ifTrue: [
		^ self markCycleWith: anInteger
	].
	^ OrderedCollection new