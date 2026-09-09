Class {
	#name : 'OCASTTranslatorMappingForFullBlockClosuresTest',
	#superclass : 'TestCase',
	#instVars : [
		'compiler'
	],
	#category : 'OpalCompiler-Tests-AST',
	#package : 'OpalCompiler-Tests',
	#tag : 'AST'
}

{ #category : 'tests' }
OCASTTranslatorMappingForFullBlockClosuresTest >> assertBlockNodeHasCorrectIR: blockNode [
	| sequence |
	blockNode methodNode ir.
	sequence := blockNode ir startSequence sequence.
	self assert: (sequence allSatisfy: [ :ir | ir sourceNode isNotNil ]).
	self assert: sequence last sourceNode equals: blockNode body
]

{ #category : 'tests' }
OCASTTranslatorMappingForFullBlockClosuresTest >> assertMethodReturnValue: method [
	self
		assert: (method valueWithReceiver: nil)
		equals: 42
]

{ #category : 'running' }
OCASTTranslatorMappingForFullBlockClosuresTest >> setUp [
	super setUp.
	compiler := OpalCompiler new
]

{ #category : 'tests' }
OCASTTranslatorMappingForFullBlockClosuresTest >> testBlockWithParametersASTMapping [
	|method blockNode|
	method := compiler source: 'm ^[:i :j|i+j] value: 41 value: 1'; compileOrFail.
	blockNode := method ast statements first value receiver.
	self assertBlockNodeHasCorrectIR: blockNode.
	self assertMethodReturnValue: method
]

{ #category : 'tests' }
OCASTTranslatorMappingForFullBlockClosuresTest >> testSimpleBlockASTMapping [
	|method blockNode|
	method := compiler source: 'm ^[40 + 2] value'; compileOrFail.
	blockNode := method ast statements first value receiver.
	self assertBlockNodeHasCorrectIR: blockNode.
	self assertMethodReturnValue: method
]
