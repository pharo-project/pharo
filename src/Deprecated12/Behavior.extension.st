Extension { #name : 'Behavior' }

{ #category : '*Deprecated12' }
Behavior >> addSelector: selector withRecompiledMethod: compiledMethod [
	<reflection: 'Class structural modification - Selector/Method modification'>
	
	self
		deprecated: 'Use #addSelectorSilently:withMethod: instead'
		transformWith: '`@rcv addSelector: `@arg1 withRecompiledMethod: `@arg2' -> '`@rcv addSelectorSilently: `@arg1 withMethod: `@arg2'.
	^ self addSelectorSilently: selector withMethod: compiledMethod
]

{ #category : '*Deprecated12' }
Behavior >> evaluate: aString [
	self deprecated: 'You should not even use it as `self` does not impact the result' transformWith: '`@rcv evaluate: `@arg' -> '`@rcv compiler evaluate: `@arg'.
	^self compiler evaluate: aString
]
