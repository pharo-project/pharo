Extension { #name : 'Behavior' }

{ #category : '*Deprecated12' }
Behavior >> addSelector: selector withRecompiledMethod: compiledMethod [

	self
		deprecated: 'Use #addSelectorSilently:withMethod: instead'
		transformWith: '`@rcv addSelector: `@arg1 withRecompiledMethod: `@arg2' -> '`@rcv addSelectorSilently: `@arg1 withMethod: `@arg2'.
	^ self addSelectorSilently: selector withMethod: compiledMethod
]
