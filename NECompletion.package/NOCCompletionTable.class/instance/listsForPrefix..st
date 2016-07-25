accessing
listsForPrefix: aString 
	|  prefixes |
	aString isEmpty ifTrue: [^ Array with: (NOCEntryList ofSize: numberofEntries)].
	prefixes := OrderedCollection with: aString first asLowercase asString.
	 
	aString size > 1 
			ifTrue: [prefixes add: ((aString first: 2) collect: [:e | e asLowercase])].
	^ prefixes collect: [:prefix | table 
		at: prefix
		ifAbsentPut: [ NOCEntryList ofSize: numberofEntries ]]