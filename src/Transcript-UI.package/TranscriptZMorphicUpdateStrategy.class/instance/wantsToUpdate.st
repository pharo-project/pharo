testing
wantsToUpdate

	^Time millisecondClockValue >= nextUpdate and: [
		self isInMorphicUIThread and: [ 
			constraintsChecker 
				value: updateConstraints
				value: [ :each | each value: self ]
		]
	]