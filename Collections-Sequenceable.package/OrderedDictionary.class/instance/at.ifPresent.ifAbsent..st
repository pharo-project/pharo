accessing
at: aKey ifPresent: aPresentBlock ifAbsent: anAbsentBlock
	^ dictionary
		at: aKey
		ifPresent: aPresentBlock
		ifAbsent: anAbsentBlock