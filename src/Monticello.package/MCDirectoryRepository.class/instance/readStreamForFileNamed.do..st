i/o
readStreamForFileNamed: aString do: aBlock
	| val |
	directory / aString readStreamDo: [ :stream|
		val := aBlock value: stream ].
	^ val