Class {
	#name : 'OCBytecodeGeneratorTest',
	#superclass : 'TestCase',
	#category : 'OpalCompiler-Tests-Bytecode',
	#package : 'OpalCompiler-Tests',
	#tag : 'Bytecode'
}

{ #category : 'accessing' }
OCBytecodeGeneratorTest class >> packageNamesUnderTest [
	^ #('OpalCompiler')
]

{ #category : 'helper' }
OCBytecodeGeneratorTest >> bytecodeGenerator [

	^ (OCIRBytecodeGenerator newWithEncoderClass: EncoderForSistaV1)
		  compilationContext: OCCompilationContext new;
		  yourself
]

{ #category : 'tests - execution' }
OCBytecodeGeneratorTest >> testExample [
	| cm |
	cm := self bytecodeGenerator
		numArgs: 1;
		numTemps: 1;
		pushTemp: 1;
		pushInstVar: 2;
		send: #>;
		if: false goto: #else;
		pushLiteral: 'yes';
		returnTop;
		label: #else;
		pushLiteral: 'no';
		returnTop;
		compiledMethod.
	cm methodClass: self class.
	cm selector: #test.

	self assert: (cm isKindOf: CompiledMethod).
	self assert: (cm valueWithReceiver: 2 @ 2 arguments: #(1)) equals: 'no'.
	self assert: (cm valueWithReceiver: 2 @ 2 arguments: #(3)) equals: 'yes'.
	^ cm
]

{ #category : 'tests - execution' }
OCBytecodeGeneratorTest >> testExample2 [
	| cm |
	cm := self bytecodeGenerator
		numArgs: 1;
		numTemps: 1;
		pushInstVar: 2;
		pushTemp: 1;
		send: #<;
		if: false goto: #else;
		pushLiteral: 'yes';
		returnTop;
		label: #else;
		pushLiteral: 'no';
		returnTop;
		compiledMethod.

	self assert: (cm isKindOf: CompiledMethod).
	self assert: (cm valueWithReceiver: 2 @ 2 arguments: #(1)) equals: 'no'.
	self assert: (cm valueWithReceiver: 2 @ 2 arguments: #(3)) equals: 'yes'.
	^ cm
]
