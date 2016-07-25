testing
includesPragma: aPragma
	^ (keywords includes: aPragma keyword) and: [ condition value: aPragma ]