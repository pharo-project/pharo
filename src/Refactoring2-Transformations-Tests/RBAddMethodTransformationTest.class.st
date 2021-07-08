Class {
	#name : #RBAddMethodTransformationTest,
	#superclass : #RBTransformationTest,
	#category : #'Refactoring2-Transformations-Tests'
}

{ #category : #running }
RBAddMethodTransformationTest >> setUp [

	super setUp.
	model := self abstractVariableTestData.
]

{ #category : #tests }
RBAddMethodTransformationTest >> testBadSourceCode [

	self shouldFail: (RBAddMethodTransformation
							sourceCode: 'asdf ^super ^printString'
							in: RBBasicLintRuleTestData
							withProtocols: {#accessing})
							asRefactoring
]

{ #category : #tests }
RBAddMethodTransformationTest >> testMethodAlreadyExists [

	self 
		shouldFail: (RBAddMethodTransformation
			sourceCode: 'printString ^super printString'
			in: #RBBasicLintRuleTestData
			withProtocols: {#accessing})
			asRefactoring;
		shouldFail: (RBAddMethodTransformation 
			model: model
			sourceCode: 'classVarName1 ^super printString'
			in: (model classNamed: #Bar)
			withProtocols: {#accessing})
			asRefactoring;
		shouldFail: (RBAddMethodTransformation
			model: model
			sourceCode: 'printString ^super printString'
			in: (model classNamed: #Bar)
			withProtocols: {#accessing})
			asRefactoring.
]

{ #category : #tests }
RBAddMethodTransformationTest >> testModel [

	| class |
	class := model metaclassNamed: #Bar.
	
	(RBAddMethodTransformation 
		model: model
		sourceCode: 'printString1 ^super printString'
		in: class
		withProtocols: {#accessing})
		asRefactoring transform.
		
	self assert: (class parseTreeFor: #printString1) 
		  equals: (self parseMethod: 'printString1 ^super printString')
]

{ #category : #tests }
RBAddMethodTransformationTest >> testRefactoring [

	| refactoring |
	refactoring := (RBAddMethodTransformation 
						sourceCode: 'printString1 ^super printString'
						in: #RBBasicDummyLintRuleTest
						withProtocols: {#accessing})
						asRefactoring transform.
	
	self assert: ((refactoring model classNamed: #RBBasicDummyLintRuleTest) parseTreeFor: #printString1)
		  equals: (self parseMethod: 'printString1 ^super printString')
]

{ #category : #tests }
RBAddMethodTransformationTest >> testTransform [

	| transformation class |
	transformation := (RBAddMethodTransformation
		sourceCode: 'printString1 ^super printString'
		in: self changeMock name
		withProtocols: {#accessing})
		transform.
	
	class := transformation model classNamed: self changeMock name.
	self assert: (class parseTreeFor: #printString1)
		  equals: (self parseMethod: 'printString1 ^super printString')
]
