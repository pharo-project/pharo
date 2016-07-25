debugger access
tempsAndValuesLimitedTo: sizeLimit indent: indent
	"Return a string of the temporary variabls and their current values"

	^ String streamContents: [ :aStream |
		self tempNames doWithIndex: [ :title :index |
			indent timesRepeat: [ aStream tab ].
			aStream nextPutAll: title; nextPut: $:; space; tab.
			aStream nextPutAll: 
				((self namedTempAt: index) printStringLimitedTo: (sizeLimit -3 -title size max: 1)).
			aStream cr ]].