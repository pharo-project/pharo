Class {
	#name : #OCASTTranslatorTest,
	#superclass : #ParametrizedTestCase,
	#instVars : [
		'instance',
		'method',
		'globals',
		'encoder',
		'fullblocks',
		'optimization'
	],
	#category : #'OpalCompiler-Tests-AST'
}

{ #category : #'building suites' }
OCASTTranslatorTest class >> testParameters [

	^ super testParameters
		addCase: { #encoder -> EncoderForV3PlusClosures };
		addCase: { #encoder -> EncoderForSistaV1. #fullblocks -> true };
		addCase: { #encoder -> EncoderForSistaV1. #fullblocks -> false }
]

{ #category : #accessing }
OCASTTranslatorTest >> compilationContext [

	| context |
	context := CompilationContext new
		encoderClass: self encoder;
		environment: globals;
		interactive: true;
		yourself.
	fullblocks ifTrue: [ context setOptions: { #optionFullBlockClosure . optimization } ].
	^ context
]

{ #category : #support }
OCASTTranslatorTest >> compileExample: aSelector [
	
	^ self compileSource: (OCOpalExamples>> aSelector) sourceCode
]

{ #category : #support }
OCASTTranslatorTest >> compileSource: source [
	| ir ast |
	ast := RBParser parseMethod: source.
	ast compilationContext: self compilationContext.
	ir := (ast doSemanticAnalysisIn: OCOpalExamples) ir.
	^ 	method := ir compiledMethod
]

{ #category : #accessing }
OCASTTranslatorTest >> encoder [

	^ encoder
]

{ #category : #accessing }
OCASTTranslatorTest >> encoder: anEncoder [

	encoder := anEncoder
]

{ #category : #support }
OCASTTranslatorTest >> executeMethod: aMethod onReceiver: receiver [

	^ aMethod valueWithReceiver: receiver arguments: #()
]

{ #category : #support }
OCASTTranslatorTest >> executeMethod: aMethod onReceiver: receiver withArgument: anArgument [

	^ self executeMethod: aMethod onReceiver: receiver withArguments: {anArgument}
]

{ #category : #support }
OCASTTranslatorTest >> executeMethod: aMethod onReceiver: receiver withArguments: arguments [

	^ aMethod valueWithReceiver: receiver arguments: arguments
]

{ #category : #support }
OCASTTranslatorTest >> executeMethodOnInstance: aMethod [

	^ self executeMethod: aMethod onReceiver: instance
]

{ #category : #accessing }
OCASTTranslatorTest >> fullblocks [
	^ fullblocks
]

{ #category : #accessing }
OCASTTranslatorTest >> fullblocks: anObject [
	fullblocks := anObject
]

{ #category : #accessing }
OCASTTranslatorTest >> globals [
	^ globals
]

{ #category : #accessing }
OCASTTranslatorTest >> globals: anObject [
	globals := anObject
]

{ #category : #initialization }
OCASTTranslatorTest >> initialize [

	super initialize.
	fullblocks := false.
]

{ #category : #accessing }
OCASTTranslatorTest >> instance [
	^ instance
]

{ #category : #accessing }
OCASTTranslatorTest >> instance: anObject [
	instance := anObject
]

{ #category : #support }
OCASTTranslatorTest >> instanceVariablesToKeep [

	^ super instanceVariablesToKeep , #('fullblocks')
]

{ #category : #accessing }
OCASTTranslatorTest >> method [
	^ method
]

{ #category : #accessing }
OCASTTranslatorTest >> method: anObject [
	^ method := anObject
]

{ #category : #accessing }
OCASTTranslatorTest >> optimization [
	^ optimization
]

{ #category : #accessing }
OCASTTranslatorTest >> optimization: anObject [
	optimization := anObject
]

{ #category : #running }
OCASTTranslatorTest >> setUp [

	super setUp.
	instance := OCOpalExamples new.
	globals := Dictionary new.
]

{ #category : #running }
OCASTTranslatorTest >> tearDown [

	OCOpalExamples reset.
	super tearDown
]

{ #category : #support }
OCASTTranslatorTest >> testExample: aSelector [

	^ self testExample: aSelector withArguments: #()
]

{ #category : #support }
OCASTTranslatorTest >> testExample: aSelector withArgument: anArgument [

	^ self testExample: aSelector withArguments: { anArgument }
]

{ #category : #support }
OCASTTranslatorTest >> testExample: aSelector withArguments: arguments [

	^ self testSource: ((OCOpalExamples>> aSelector) sourceCode) withArguments: arguments

]

{ #category : #'testing - primitives' }
OCASTTranslatorTest >> testExamplePrimitiveErrorCode [
	| method ast ir newMethod |
	method := OCOpalExamples >> #examplePrimitiveErrorCode.
	ast := method parseTree.
	ir := (ast doSemanticAnalysisIn: OCOpalExamples) ir.

	newMethod := ir compiledMethod.
	self assert: method primitive equals: newMethod primitive
]

{ #category : #'testing - primitives' }
OCASTTranslatorTest >> testExamplePrimitiveErrorCodeModule [
	| method ast ir newMethod |
	method := OCOpalExamples >> #examplePrimitiveErrorCodeModule.
	ast := method parseTree.
	ir := (ast doSemanticAnalysisIn: OCOpalExamples) ir.

	newMethod := ir compiledMethod.

	self assert: method primitive equals: newMethod primitive.
	self assert: method pragmas printString equals: newMethod pragmas printString
]

{ #category : #'testing - primitives' }
OCASTTranslatorTest >> testExamplePrimitiveErrorModule [
"test for primitive declaration
  <primitive: 'primFunction'  error: errorCode module: 'primModule'  >
"
	| method ast ir |
	method := OCOpalExamples >> #examplePrimitiveErrorModule.
	ast := method parseTree.
	ir := (ast doSemanticAnalysisIn: OCOpalExamples) ir.
	self assert: ir tempKeys equals: #(#errorCode)
]

{ #category : #'testing - primitives' }
OCASTTranslatorTest >> testExamplePrimitiveModuleError [
"teset for primitive declaration
  <primitive: 'primFunction'  module: 'primModule' error: errorCode >
"
	| method ast ir |
	method := OCOpalExamples >> #examplePrimitiveModuleError.
	ast := method parseTree.
	ir := (ast doSemanticAnalysisIn: OCOpalExamples) ir.
	self assert: ir tempKeys equals: #(#errorCode)
]

{ #category : #'testing - simple' }
OCASTTranslatorTest >> testSendSpecialBinaryMessage [

	self assert: (Smalltalk specialSelectors includes: #+).
	self assert: (self testExample: #exampleReturn1plus2) equals: 3
]

{ #category : #support }
OCASTTranslatorTest >> testSource: sourceCode [

	^ self testSource: sourceCode withArguments: #()
]

{ #category : #support }
OCASTTranslatorTest >> testSource: sourceCode withArguments: arguments [

	self compileSource: sourceCode.
	^ self executeMethod: method onReceiver: instance withArguments: arguments
]
