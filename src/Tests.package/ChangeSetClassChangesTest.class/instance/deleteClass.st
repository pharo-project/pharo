support
deleteClass
	| cl |
	cl := Smalltalk globals at: className ifAbsent: [ ^ self ].
	cl
		removeFromChanges;
		removeFromSystemUnlogged