"
I have tests for the curious capability of Opal to compile code with syntax errors.

Instead of raising syntax errors at runtime, they are raised at compile time.


"
Class {
	#name : #OCCompileWithFailureTest,
	#superclass : #TestCase,
	#category : #'OpalCompiler-Tests-Source'
}

{ #category : #tests }
OCCompileWithFailureTest >> testEvalSimpleMethodWithError [
	| ast cm |
	ast := OpalCompiler new
				source: 'method 3+';
				options: #(+ optionParseErrors);
				parse.
	
	self assert: ast isMethod.
	self assert: ast isFaulty.
	
	cm := ast compiledMethod.
	self should: [cm valueWithReceiver: nil arguments: #()] raise: SyntaxErrorNotification
]
