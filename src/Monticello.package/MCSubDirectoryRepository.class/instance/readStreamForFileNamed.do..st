i/o
readStreamForFileNamed: aString do: aBlock
	| file val |
	file := FileStream readOnlyFileNamed: (self findFullNameForReading: aString).
	val := aBlock value: file.
	file close.
	^ val