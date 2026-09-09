"
I have tests for the curious capability of Opal to compile code with syntax errors.

Instead of raising syntax errors at runtime, they are raised at compile time.


"
Class {
	#name : 'OCCompileWithFailureTest',
	#superclass : 'TestCase',
	#category : 'OpalCompiler-Tests-Source',
	#package : 'OpalCompiler-Tests',
	#tag : 'Source'
}

{ #category : 'tests' }
OCCompileWithFailureTest >> testEmptyBlockArg [
	| result |
	"parse [:] in parse error mode, this results in arg vars with an empty string as a name"
	result := UndefinedObject compiler
    source: '^[ :]';
    isScripting: true;
    parse.
	self assert: result isDoIt.
	self assert: result statements first isReturn
]

{ #category : 'tests' }
OCCompileWithFailureTest >> testEvalSimpleMethodWithError [
	| compiler cm |
	cm := (compiler := OpalCompiler new)
				source: 'method 3+';
				permitFaulty: true;
				compileOrFail.

	self assert: compiler ast isMethod.
	self assert: compiler ast isFaulty.

	self should: [cm valueWithReceiver: nil] raise: OCRuntimeSyntaxError
]

{ #category : 'tests' }
OCCompileWithFailureTest >> testParenthesis [
	| compiler cm |
	cm := (compiler := UndefinedObject compiler)
    source: 'self assert: pragma( numArgs equals: 0.';
    isScripting: true;
    permitFaulty: true;
    compileOrFail.
	self assert: compiler ast isDoIt.
	self assert: compiler ast isFaulty.
	self should: [cm valueWithReceiver: nil] raise: OCRuntimeSyntaxError
]
