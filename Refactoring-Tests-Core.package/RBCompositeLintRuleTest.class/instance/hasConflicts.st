testing
hasConflicts
	^(rules detect: [:each | each hasConflicts] ifNone: [nil]) notNil