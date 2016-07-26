instance creation
before: beforeSymbol after: afterSymbol

	^ self new
		initializeBefore: beforeSymbol
		           after: afterSymbol;
		yourself