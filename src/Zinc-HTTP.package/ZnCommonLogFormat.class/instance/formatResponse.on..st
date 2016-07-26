private
formatResponse: event on: stream
	| size |
	stream space.
	stream print: event response code.
	stream space.
	size := event response hasEntity 
		ifTrue: [ event response contentLength ] 
		ifFalse: [ 0 ].
	stream print: size
