accessing
abbreviation
	^ String streamContents: [ :s |
		s nextPutAll: 'LT'; print: self offset hours; nextPut: $:.
		s nextPutAll: (self offset minutes printPaddedWith: $0 to: 2) ].