helper
unicodeCategoryTableLookup: codePoint
	| index table |
	index := codePoint + 1.
	table := Unicode generalCategory.
	^ index > table size
		ifTrue: [ -1 ]
		ifFalse: [ table at: index ]