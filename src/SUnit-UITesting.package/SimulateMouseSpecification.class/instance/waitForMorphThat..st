helpers
waitForMorphThat: aBlock

	self timeoutAfter: 2 seconds ifNot: [
		morph := World submorphThat: aBlock ifNone: [ nil ].
		(Delay forMilliseconds: 100) wait.
		morph isNil].
	^ morph.