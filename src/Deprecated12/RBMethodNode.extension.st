Extension { #name : 'RBMethodNode' }

{ #category : '*Deprecated12' }
RBMethodNode >> generate [

	self deprecated: 'Use generateMethod. Beware that there are limitations so read the comments.' transformWith: '`@receiver generate' -> '`@receiver generateMethod'.

	^ self generateMethod
]

{ #category : '*Deprecated12' }
RBMethodNode >> generate: trailer [
	self
		deprecated: 'Use generateMethod'
		transformWith: '`@receiver generate: `@argument'
			-> '`@receiver generateMethod'.
	^ self generateMethod
]

{ #category : '*Deprecated12' }
RBMethodNode >> method [
	
	self deprecated: 'use compiledMethod' transformWith: '`@recv method' -> '`@recv compiledMethod'.
	^ self compiledMethod
]
