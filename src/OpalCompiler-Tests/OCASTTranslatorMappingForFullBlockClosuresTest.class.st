Class {
	#name : #OCASTTranslatorMappingForFullBlockClosuresTest,
	#superclass : #TestCase,
	#instVars : [
		'ctx',
		'compiler'
	],
	#category : #'OpalCompiler-Tests-AST'
}

{ #category : #testing }
OCASTTranslatorMappingForFullBlockClosuresTest >> assertBlockNodeHasCorrectIR: blockNode [
	| sequence |
	blockNode methodNode ir.
	sequence := blockNode ir startSequence sequence.
	self assert: (sequence allSatisfy: [ :ir | ir sourceNode notNil ]).
	self assert: sequence last sourceNode equals: blockNode body
]

{ #category : #testing }
OCASTTranslatorMappingForFullBlockClosuresTest >> assertMethodReturnValue: method [
	self
		assert: (method valueWithReceiver: nil arguments: #())
		equals: 42
]

{ #category : #support }
OCASTTranslatorMappingForFullBlockClosuresTest >> compileSourceToMethod: source [
	| method |
	method := compiler
		source: source;
		compile.
	method ast compilationContext: ctx.
	^ method
]

{ #category : #running }
OCASTTranslatorMappingForFullBlockClosuresTest >> setUp [
	super setUp.
	ctx := CompilationContext new
		encoderClass: EncoderForSistaV1;
		setOptions: {#optionFullBlockClosure . #optionEmbeddSources} asSet.
	compiler := OpalCompiler new
		compilationContext: ctx;
		yourself
]

{ #category : #running }
OCASTTranslatorMappingForFullBlockClosuresTest >> tearDown [
	super tearDown 
]

{ #category : #testing }
OCASTTranslatorMappingForFullBlockClosuresTest >> testBlockWithParametersASTMapping [
	|method blockNode|
	method := self compileSourceToMethod: 'm ^[:i :j|i+j] value: 41 value: 1'.
	blockNode := method ast statements first value receiver.
	self assertBlockNodeHasCorrectIR: blockNode.
	self assertMethodReturnValue: method
]

{ #category : #testing }
OCASTTranslatorMappingForFullBlockClosuresTest >> testSimpleBlockASTMapping [
	|method blockNode|
	method := self compileSourceToMethod: 'm ^[40 + 2] value'.
	blockNode := method ast statements first value receiver.
	self assertBlockNodeHasCorrectIR: blockNode.
	self assertMethodReturnValue: method
]
