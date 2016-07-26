accessing
contentStream
	"Answer a RWBinaryOrTextStream on the contents."

	contentStream
		ifNil: [contentStream := contents
				ifNil: [self contentStreamOnURI]
				ifNotNil: [(RWBinaryOrTextStream with: self contents) reset]].
	^contentStream