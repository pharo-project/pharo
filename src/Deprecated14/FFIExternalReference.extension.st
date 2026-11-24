Extension { #name : 'FFIExternalReference' }

{ #category : '*Deprecated14' }
FFIExternalReference >> handle [
	self
		deprecated: 'Use #getHandle instead'
		in: #Pharo14
		transformWith: '`@receiver handle' -> '`@receiver getHandle'.
	^ self getHandle
]

{ #category : '*Deprecated14' }
FFIExternalReference >> handle: anObject [

	self
		deprecated: 'Please use #setHandle: instead'
		in: #Pharo14
		transformWith: '`@receiver handle: `@arg' -> '`@receiver setHandle: `@arg'.

	self setHandle: anObject
]
