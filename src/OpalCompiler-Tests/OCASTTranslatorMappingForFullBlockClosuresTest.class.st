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
	ctx := CompilationContext new
		encoderClass: EncoderForSistaV1;
		setOptions: {#optionFullBlockClosure . #optionEmbeddSources} asSet.
	compiler := OpalCompiler new
		compilationContext: ctx;
		yourself
]

{ #category : #testing }
OCASTTranslatorMappingForFullBlockClosuresTest >> testSimpleBlockASTMapping [
	|method blockNode|
	method := self compileSourceToMethod: 'm [1 + 2] value'.
	blockNode := method ast statements first receiver.
	self assertBlockNodeHasCorrectIR: blockNode
]
