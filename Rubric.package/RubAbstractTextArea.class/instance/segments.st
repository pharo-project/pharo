accessing segments
segments
	^ segments ifNil: [ segments := OrderedCollection new ]
