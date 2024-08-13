Class {
	#name : 'OCASTTranslatorTest',
	#superclass : 'ParametrizedTestCase',
	#instVars : [
		'instance',
		'method',
		'globals',
		'encoder',
		'optimization'
	],
	#category : 'OpalCompiler-Tests-AST',
	#package : 'OpalCompiler-Tests',
	#tag : 'AST'
}

{ #category : 'building suites' }
OCASTTranslatorTest class >> inliningOptionsFor: enableOption [

	^ {
		  #(  ). "No options supplied"
		  { enableOption }. "Explicitly enable this type of inlining"
		  #( #optionInlineNone ). "Disable all inlining"
		  "Disable all inlining, *but* the specific option is also set (and should be ignored). See bug #14323."
		  { enableOption. #optionInlineNone }}
]

{ #category : 'building suites' }
OCASTTranslatorTest class >> testParameters [

	^ super testParameters
		addCase: { #encoder -> EncoderForSistaV1 };
		yourself
]

{ #category : 'accessing' }
OCASTTranslatorTest >> compilationContext [

	| context |
	context := CompilationContext new
		           encoderClass: self encoder;
		           bindings: globals;
		           yourself.
	self optimization ifNotNil: [ :a | context parseOptions: #( #+ ) , a ].
	^ context
]

{ #category : 'support' }
OCASTTranslatorTest >> compileExample: aSelector [

	^ self compileSource: (OCOpalExamples>> aSelector) sourceCode
]

{ #category : 'support' }
OCASTTranslatorTest >> compileSource: source [
	| ir ast |
	ast := RBParser parseMethod: source.
	ast compilationContext: self compilationContext.
	ast compiler
		class: OCOpalExamples;
		bindings: globals;
		doSemanticAnalysis.
	ast checkFaulty: nil.
	ir := ast ir.
	^ 	method := ir compiledMethod
]

{ #category : 'accessing' }
OCASTTranslatorTest >> encoder [

	^ encoder
]

{ #category : 'accessing' }
OCASTTranslatorTest >> encoder: anEncoder [

	encoder := anEncoder
]

{ #category : 'support' }
OCASTTranslatorTest >> executeMethod: aMethod onReceiver: receiver [

	^ aMethod valueWithReceiver: receiver
]

{ #category : 'support' }
OCASTTranslatorTest >> executeMethod: aMethod onReceiver: receiver withArgument: anArgument [

	^ self executeMethod: aMethod onReceiver: receiver withArguments: {anArgument}
]

{ #category : 'support' }
OCASTTranslatorTest >> executeMethod: aMethod onReceiver: receiver withArguments: arguments [

	^ aMethod valueWithReceiver: receiver arguments: arguments
]

{ #category : 'support' }
OCASTTranslatorTest >> executeMethodOnInstance: aMethod [

	^ self executeMethod: aMethod onReceiver: instance
]

{ #category : 'accessing' }
OCASTTranslatorTest >> globals [
	^ globals
]

{ #category : 'accessing' }
OCASTTranslatorTest >> globals: anObject [
	globals := anObject
]

{ #category : 'accessing' }
OCASTTranslatorTest >> instance [
	^ instance
]

{ #category : 'accessing' }
OCASTTranslatorTest >> instance: anObject [
	instance := anObject
]

{ #category : 'support' }
OCASTTranslatorTest >> instanceVariablesToKeep [

	^ super instanceVariablesToKeep , #('fullblocks')
]

{ #category : 'accessing' }
OCASTTranslatorTest >> method [
	^ method
]

{ #category : 'accessing' }
OCASTTranslatorTest >> method: anObject [
	^ method := anObject
]

{ #category : 'accessing' }
OCASTTranslatorTest >> optimization [
	^ optimization
]

{ #category : 'accessing' }
OCASTTranslatorTest >> optimization: anObject [
	optimization := anObject
]

{ #category : 'running' }
OCASTTranslatorTest >> setUp [

	super setUp.
	instance := OCOpalExamples new.
	globals := SystemEnvironment new
]

{ #category : 'running' }
OCASTTranslatorTest >> tearDown [

	OCOpalExamples reset.
	super tearDown
]

{ #category : 'support' }
OCASTTranslatorTest >> testExample: aSelector [

	^ self testExample: aSelector withArguments: #()
]

{ #category : 'support' }
OCASTTranslatorTest >> testExample: aSelector withArgument: anArgument [

	^ self testExample: aSelector withArguments: { anArgument }
]

{ #category : 'support' }
OCASTTranslatorTest >> testExample: aSelector withArguments: arguments [

	^ self testSource: ((OCOpalExamples>> aSelector) sourceCode) withArguments: arguments
]

{ #category : 'tests - primitives' }
OCASTTranslatorTest >> testExamplePrimitiveErrorCode [
	| ast ir newMethod |
	method := OCOpalExamples >> #examplePrimitiveErrorCode.
	ast := method parseTree.
	ir := ast ir.

	newMethod := ir compiledMethod.
	self assert: method primitive equals: newMethod primitive
]

{ #category : 'tests - primitives' }
OCASTTranslatorTest >> testExamplePrimitiveErrorCodeModule [
	| ast ir newMethod |
	method := OCOpalExamples >> #examplePrimitiveErrorCodeModule.
	ast := method parseTree.
	ir := ast ir.

	newMethod := ir compiledMethod.

	self assert: method primitive equals: newMethod primitive.
	self assert: method pragmas printString equals: newMethod pragmas printString
]

{ #category : 'tests - primitives' }
OCASTTranslatorTest >> testExamplePrimitiveErrorModule [
"test for primitive declaration
  <primitive: 'primFunction'  error: errorCode module: 'primModule'  >
"
	| ast ir |
	method := OCOpalExamples >> #examplePrimitiveErrorModule.
	ast := method parseTree.
	ir := ast ir.
	self assert: ir tempKeys equals: #(#errorCode)
]

{ #category : 'tests - primitives' }
OCASTTranslatorTest >> testExamplePrimitiveModuleError [
"teset for primitive declaration
  <primitive: 'primFunction'  module: 'primModule' error: errorCode >
"
	| ast ir |
	method := OCOpalExamples >> #examplePrimitiveModuleError.
	ast := method parseTree.
	ir := ast ir.
	self assert: ir tempKeys equals: #(#errorCode)
]

{ #category : 'support' }
OCASTTranslatorTest >> testSource: sourceCode [

	^ self testSource: sourceCode withArguments: #()
]

{ #category : 'support' }
OCASTTranslatorTest >> testSource: sourceCode withArguments: arguments [

	self compileSource: sourceCode.
	^ self executeMethod: method onReceiver: instance withArguments: arguments
]
