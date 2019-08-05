Class {
	#name : #OCASTBasicTranslatorTest,
	#superclass : #OCASTTranslatorTest,
	#category : #'OpalCompiler-Tests-AST'
}

{ #category : #'testing - simple' }
OCASTBasicTranslatorTest >> testEmptyMethodReturnsReceiver [

	self assert: (self testExample: #exampleEmptyMethod) equals: instance
]

{ #category : #'testing - simple' }
OCASTBasicTranslatorTest >> testInitialInstanceIsEmpty [

	self assert: instance result equals: nil
]

{ #category : #'testing - errors' }
OCASTBasicTranslatorTest >> testSyntaxError [
	
	self
		should: [ self compileSource: 'foo (']
		raise: SyntaxErrorNotification
]
