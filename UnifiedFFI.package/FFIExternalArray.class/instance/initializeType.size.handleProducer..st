initialization
initializeType: aType size: aNumber handleProducer: aBlock 
	type := aType.
	size := aNumber.
	handle := aBlock value: (size 
		ifNotNil: [ size * type typeSize ]
		ifNil: [ 0 ]).
	handle isNull ifTrue: [ self error: 'External allocation failed' ].
	self initialize