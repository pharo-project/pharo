accessing
stream
	stream closed ifTrue: [ stream open ].
	^ stream
	