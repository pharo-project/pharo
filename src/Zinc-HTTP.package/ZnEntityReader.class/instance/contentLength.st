accessing
contentLength
	^ (self headers includesKey: 'Content-Length')
		ifTrue: [ self headers contentLength ]
		ifFalse: [ nil ]