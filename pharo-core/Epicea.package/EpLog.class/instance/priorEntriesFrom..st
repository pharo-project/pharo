enumerating
priorEntriesFrom: aReference

	^ Array streamContents: [  :stream |
		self priorEntriesFrom: aReference do: [ :anEntry |
			stream nextPut: anEntry ] ]