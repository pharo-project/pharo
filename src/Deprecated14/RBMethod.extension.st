Extension { #name : 'RBMethod' }

{ #category : '*Deprecated14' }
RBMethod >> method [
	self
		deprecated: 'Use compiledMethod'
		in: #Pharo14
		transformWith: '`@rec method' -> '`@rec compiledMethod'.
	^compiledMethod
]

{ #category : '*Deprecated14' }
RBMethod >> method: aCompiledMethod [
	self
		deprecated: 'Use compiledMethod'
		in: #Pharo14
		transformWith: '`@rec method: `@arg' -> '`@rec compiledMethod: `@arg'.
	
	self compiledMethod: aCompiledMethod
]
