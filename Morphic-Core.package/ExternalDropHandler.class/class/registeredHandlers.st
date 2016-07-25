private
registeredHandlers
	RegisteredHandlers ifNil: [RegisteredHandlers := OrderedCollection new].
	^RegisteredHandlers