Extension { #name : 'OCLiteralValueNode' }

{ #category : '*Deprecated15' }
OCLiteralValueNode >> evaluateForContext: aContext [

	self
		deprecated: 'Use OpalEvaluator or #value instead'
		transformWith: '`@rcv evaluateForContext: `@ctx' -> '`@rcv value'.
	^ value
]
