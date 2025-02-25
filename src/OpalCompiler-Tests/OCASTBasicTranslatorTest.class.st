Class {
	#name : 'OCASTBasicTranslatorTest',
	#superclass : 'OCASTTranslatorTest',
	#category : 'OpalCompiler-Tests-AST',
	#package : 'OpalCompiler-Tests',
	#tag : 'AST'
}

{ #category : 'tests - simple' }
OCASTBasicTranslatorTest >> testEmptyMethodReturnsReceiver [

	self assert: (self testExample: #exampleEmptyMethod) equals: instance
]

{ #category : 'tests - simple' }
OCASTBasicTranslatorTest >> testInitialInstanceIsEmpty [

	self assert: instance result equals: nil
]

{ #category : 'tests - errors' }
OCASTBasicTranslatorTest >> testSyntaxError [

	self
		should: [ self compileSource: 'foo (']
		raise: OCCodeError
]
