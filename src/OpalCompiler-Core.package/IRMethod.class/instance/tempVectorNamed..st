enumerating
tempVectorNamed: aName
	
	^self firstInstructionMatching: [:ir | ir isTempVector and:  [ir name = aName ] ]