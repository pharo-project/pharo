Extension { #name : 'IRMethod' }

{ #category : '*Deprecated12' }
IRMethod >> compiledMethodWith: trailer [
	self
		deprecated: 'Use compiledMethod'
		transformWith: '`@receiver compiledMethodWith: `@argument'
			-> '`@receiver compiledMethod'.
	^ self compiledMethod
]

{ #category : '*Deprecated12' }
IRMethod >> generate: trailer [
	self
		deprecated: 'Use generate'
		transformWith: '`@receiver generate: `@argument'
			-> '`@receiver generate'.
	^ self generate 
]
