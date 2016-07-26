private-selector
messageSearchBlockFrom: aString
	| exactMatch |
	
	exactMatch := aString first = $" and: [ aString last = $"].
	
	exactMatch ifFalse: [ 
		^ [ :method | 
			method selector includesSubstring: aString caseSensitive: false ]].
		
	 ^ (Symbol findInterned: (aString copyFrom: 2 to: aString size - 1))
		ifNil: [  nil ]
		ifNotNil: [ :aSymbol | 
			[ :method | method selector = aSymbol ]].