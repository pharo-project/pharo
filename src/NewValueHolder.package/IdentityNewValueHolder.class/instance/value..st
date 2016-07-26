accessing
value: anObject

	self value == anObject ifTrue: [ ^ anObject ].
	super value: anObject