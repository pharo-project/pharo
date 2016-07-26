building
sortKeyFor: aDefinition
	^ String streamContents:
		[:s |
		aDefinition description
			do: [:ea | s nextPutAll: ea asString]
			separatedBy: [s nextPut: $.]]