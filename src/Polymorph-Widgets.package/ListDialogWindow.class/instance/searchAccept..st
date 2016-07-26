morphic protocol
searchAccept: aString
	self searchUpdate: aString.
	list detect: [ :item | (self displayItem: item) = aString ] ifFound: [ :item | ^ self accept: item ].
	acceptNewEntry
		ifTrue: [ ^ self accept: aString ]
		ifFalse: [ 
			list size = 1
				ifTrue: [ ^ self accept: list first ] ].
	list ifNotEmpty: [ ^ self giveFocusToList ]