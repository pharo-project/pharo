morphic ui
orderString: anIndex
	^String streamContents: [ :stream |
		order = anIndex
			ifTrue: [ stream nextPutAll: '<yes>' ]
			ifFalse: [ stream nextPutAll: '<no>' ].
		stream nextPutAll: (self orderSpecs at: anIndex) key ]