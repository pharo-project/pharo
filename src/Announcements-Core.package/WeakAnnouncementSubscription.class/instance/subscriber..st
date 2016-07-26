accessing
subscriber: anObject

	self subscriber ifNotNil: [ self error: 'subscriber already set' ].

	self basicAt: 1 put: anObject