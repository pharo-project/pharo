private
label: aString forSuite: aTestSuite
	^ String streamContents: [ :stream |
		stream nextPutAll: 'Running '; print: aTestSuite tests size; space; nextPutAll: aString.
		aTestSuite tests size > 1 ifTrue: [ stream nextPut: $s ] ]. 