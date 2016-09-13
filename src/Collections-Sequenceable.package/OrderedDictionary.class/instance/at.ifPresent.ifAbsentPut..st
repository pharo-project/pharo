accessing
at: aKey ifPresent: aPresentBlock ifAbsentPut: anAbsentBlock
	^ dictionary
		at: aKey
		ifPresent: aPresentBlock
		ifAbsentPut: anAbsentBlock