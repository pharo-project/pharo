testing
hasIncompleteIdentifier
	^ self binding isUndeclared
		ifTrue: [ (self scope findVariable: [ :v | v name beginsWith: self name ] ifNone: [ nil ]) notNil]