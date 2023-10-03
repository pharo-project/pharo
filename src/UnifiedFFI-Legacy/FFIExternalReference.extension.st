Extension { #name : 'FFIExternalReference' }

{ #category : '*UnifiedFFI-Legacy' }
FFIExternalReference >> handle [
	self deprecated: 'Use #getHandle instead' transformWith: '`@receiver handle' -> '`@receiver getHandle'.
	^ self getHandle
]

{ #category : '*UnifiedFFI-Legacy' }
FFIExternalReference >> handle: anObject [

	self
		deprecated: 'Please use #setHandle: instead'
		transformWith:
		'`@receiver handle: `@arg' -> '`@receiver setHandle: `@arg'.

	self setHandle: anObject
]
