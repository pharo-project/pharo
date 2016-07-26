installing
stringForSortedVariablesOfType: aSymbol
	^ String streamContents:
		[:stream |
		(self selectVariables: aSymbol) asSortedCollection
			do: [:ea | stream nextPutAll: ea]
			separatedBy: [stream space]]