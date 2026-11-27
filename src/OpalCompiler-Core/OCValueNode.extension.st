Extension { #name : 'OCValueNode' }

{ #category : '*OpalCompiler-Core' }
OCValueNode >> evaluate [
	"evaluate the AST with a nil receiver"
	^ self evaluateForReceiver: nil
]

{ #category : '*OpalCompiler-Core' }
OCValueNode >> evaluateForReceiver: aReceiver [
	"evaluate the AST binding self to the receiver and taking its variables"

	^ OpalEvaluator new
		  receiver: aReceiver;
		  evaluate: self formattedCode
]
