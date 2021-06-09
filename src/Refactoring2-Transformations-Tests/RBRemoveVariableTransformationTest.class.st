Class {
	#name : #RBRemoveVariableTransformationTest,
	#superclass : #RBTransformationTest,
	#category : #'Refactoring2-Transformations-Tests'
}

{ #category : #tests }
RBRemoveVariableTransformationTest >> testModelRemoveInstanceVariable [

	| class |
	model 	defineClass: 'nil subclass: #Object
		instanceVariableNames: ''foo1''
		classVariableNames: ''''
		poolDictionaries: ''''
		category: #''Refactory-Test data'''.
	class := model classNamed: #Object.
	
	self assert: (class definesInstanceVariable: 'foo1').
	(RBRemoveVariableTransformation
		model: model
		instanceVariable: 'foo1'
		class: class)
		asRefactoring transform.
	self deny: (class definesInstanceVariable: 'foo1')
]

{ #category : #tests }
RBRemoveVariableTransformationTest >> testNonExistantName [

	self 
		shouldFail: (RBRemoveVariableTransformation 
						instanceVariable: 'name1'
						class: #RBDummyLintRuleTest)
						asRefactoring;
		shouldFail: (RBRemoveVariableTransformation 
						classVariable: #RecursiveSelfRule1
						class: #RBTransformationDummyRuleTest)
						asRefactoring
]

{ #category : #tests }
RBRemoveVariableTransformationTest >> testRefactoring [

	| refactoring class |
	refactoring := (RBRemoveVariableTransformation 
						instanceVariable: 'foo1'
						class: #RBDummyLintRuleTest)
						asRefactoring.
	class := refactoring model classNamed: #RBDummyLintRuleTest.
	
	self assert: (class definesInstanceVariable: 'foo1').
	refactoring transform.
	self deny: (class definesInstanceVariable: 'foo1')
]

{ #category : #tests }
RBRemoveVariableTransformationTest >> testReferencedVariable [

	self 
		shouldFail: (RBRemoveVariableTransformation 
						instanceVariable: 'name'
						class: #RBDummyLintRuleTest)
						asRefactoring;
		shouldFail: (RBRemoveVariableTransformation 
						classVariable: #RecursiveSelfRule
						class: #RBTransformationDummyRuleTest)
						asRefactoring 
]

{ #category : #tests }
RBRemoveVariableTransformationTest >> testRemoveClassVariable [

	| refactoring class |
	refactoring := (RBRemoveVariableTransformation
						classVariable: 'Foo1'
						class: #RBDummyLintRuleTest)
						asRefactoring.
	class := refactoring model classNamed: #RBDummyLintRuleTest.
	
	self assert: (class definesClassVariable: 'Foo1').
	refactoring transform.
	self deny: (class definesClassVariable: 'Foo1')
]

{ #category : #tests }
RBRemoveVariableTransformationTest >> testTransform [

	| transformation class |
	transformation := (RBRemoveVariableTransformation 
							instanceVariable: 'instVar'
							class: self changeMock name)
							transform. 
	
	self assert: transformation model changes changes size equals: 1.
	
	class := transformation model classNamed: self changeMock name asSymbol.
	self deny: (class directlyDefinesInstanceVariable: 'instVar').
]
