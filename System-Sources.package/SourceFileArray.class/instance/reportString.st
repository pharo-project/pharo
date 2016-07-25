printing
reportString

	^ String streamContents: [ :aStream | 
		{ self sourcesFileStream. self changesFileStream } 
			do: [:each | each ifNotNil: [
				aStream nextPutAll: each printString; cr ] ] ]