Extension { #name : 'FFIExternalReference' }

{ #category : '*Deprecated14' }
FFIExternalReference >> handle [
	self deprecated: 'Use #getHandle instead' transformWith: '`@receiver handle' -> '`@receiver getHandle'.
	^ self getHandle
]

{ #category : '*Deprecated14' }
FFIExternalReference >> handle: anObject [

	self
		deprecated: 'Please use #setHandle: instead'
		transformWith:
		'`@receiver handle: `@arg' -> '`@receiver setHandle: `@arg'.

	self setHandle: anObject
]
