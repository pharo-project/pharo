"
I have tests for the curious capability of Opal to compile code with syntax errors.

Instead of raising syntax errors at runtime, they are raised at compile time.


"
Class {
	#name : #OCCompileWithFailureTest,
	#superclass : #TestCase,
	#category : #'OpalCompiler-Tests-Source'
}

{ #category : #'interactive error protocol' }
OCCompileWithFailureTest >> notify: aMessage at: positon in: object [
	"ingnore, for testEmptyBlockArg"
]

{ #category : #tests }
OCCompileWithFailureTest >> testEmptyBlockArg [
	| result | 
	"parse [:] in parse error mode, this results in arg vars with an empty string as a name"
	result := UndefinedObject compiler
    source: '^[ :]';
    noPattern: true;
    options:  #(+ optionParseErrors + optionSkipSemanticWarnings);
    requestor: self;
    parse. 
	self assert: result isDoIt.
	self assert: result statements first isReturn
]

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
	self should: [cm valueWithReceiver: nil arguments: #()] raise: RuntimeSyntaxError
]
