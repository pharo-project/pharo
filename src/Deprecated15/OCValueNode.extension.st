Extension { #name : 'OCValueNode' }

{ #category : '*Deprecated15' }
OCValueNode >> evaluateForContext: aContext [
	"evaluate the AST taking variables from the context"

	self
		deprecated: 'Use OpalEvaluator instead'
		transformWith: '`@rcv evaluateForContext: `@ctx' -> '(OpalEvaluator new context: `@ctx; evaluate: `@rcv formattedCode)'.
	^ OpalEvaluator new
		  context: aContext;
		  evaluate: self formattedCode
]
