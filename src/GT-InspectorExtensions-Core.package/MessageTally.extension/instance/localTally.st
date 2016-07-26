*GT-InspectorExtensions-Core
localTally
	^ self children 
		inject: self tally
		into: [ :diff :each | diff - each tally ]