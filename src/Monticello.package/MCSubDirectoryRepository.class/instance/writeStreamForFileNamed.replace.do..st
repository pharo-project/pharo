i/o
writeStreamForFileNamed: aString replace: aBoolean do: aBlock 
	| file |
	file := aBoolean
				ifTrue: [FileStream
						forceNewFileNamed: (self findFullNameForReading: aString)]
				ifFalse: [FileStream
						newFileNamed: (self findFullNameForWriting: aString)].
	aBlock value: file.
	file close