testing
isNumber: aStringOrStream

	| stream |
	stream := aStringOrStream readStream.
	self parse: stream onError: [ ^ false ].
	^ stream atEnd