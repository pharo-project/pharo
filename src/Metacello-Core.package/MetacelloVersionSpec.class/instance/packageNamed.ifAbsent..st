querying
packageNamed: aString ifAbsent: aBlock

	^self packageNamed: aString forMap: self packages map ifAbsent: aBlock