support
deleteRenamedClass
	| cl |
	cl := Smalltalk globals at: renamedName ifAbsent: [ ^ self ].
	cl
		removeFromChanges;
		removeFromSystemUnlogged