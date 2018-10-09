Class {
	#name : #RBAddVariableAccessorTransformationTest,
	#superclass : #RBTransformationTest,
	#category : #'Refactoring2-Transformations-Tests'
}

{ #category : #testing }
RBAddVariableAccessorTransformationTest >> testExistingInstanceVariableAccessors [

	| refactoring |
	refactoring := (RBAddVariableAccessorTransformation 
						instanceVariable: 'name'
						class: #RBDummyLintRuleTest)
						asRefactoring transform.
		
	self assert: refactoring changes changes isEmpty
]

{ #category : #testing }
RBAddVariableAccessorTransformationTest >> testNewClassVariableAccessors [

	| refactoring class |
	refactoring := (RBAddVariableAccessorTransformation 
						classVariable: 'Foo1'
						class: #RBDummyLintRuleTest)
						asRefactoring transform.
	
	class := refactoring model metaclassNamed: #RBDummyLintRuleTest.
	self deny: refactoring changes changes isEmpty.
	self assert: (class parseTreeFor: #foo1) = (RBParser parseMethod: 'foo1 ^Foo1').
	self assert: (class parseTreeFor: #foo1:) = (RBParser parseMethod: 'foo1: anObject Foo1 := anObject')
]

{ #category : #testing }
RBAddVariableAccessorTransformationTest >> testNewInstanceVariableAccessors [

	| refactoring class |
	refactoring := (RBAddVariableAccessorTransformation
						instanceVariable: 'foo1'
						class: #RBDummyLintRuleTest)
						asRefactoring transform.
		
	class := refactoring model classNamed: #RBDummyLintRuleTest.
	self deny: refactoring changes changes isEmpty.
	self assert: (class parseTreeFor: #foo1) = (RBParser parseMethod: 'foo1 ^foo1').
	self assert: (class parseTreeFor: #foo1:) = (RBParser parseMethod: 'foo1: anObject foo1 := anObject')
]

{ #category : #testing }
RBAddVariableAccessorTransformationTest >> testNonExistantName [

	self
		shouldFail: (RBAddVariableAccessorTransformation 
						classVariable: #Foo
						class: #RBBasicLintRuleTest)
						asRefactoring;
		shouldFail: (RBAddVariableAccessorTransformation 
						instanceVariable: 'foo'
						class: #RBBasicLintRuleTest)
						asRefactoring
]

{ #category : #testing }
RBAddVariableAccessorTransformationTest >> testRefactoring [

	| refactoring class |
	refactoring := (RBAddVariableAccessorTransformation 
						instanceVariable: 'foo1'
						class: #RBDummyLintRuleTest)
						asRefactoring transform.
		
	class := refactoring model classNamed: #RBDummyLintRuleTest.
	self deny: refactoring changes changes isEmpty.
	self assert: (class parseTreeFor: #foo1) = (RBParser parseMethod: 'foo1 ^foo1').
	self assert: (class parseTreeFor: #foo1:) = (RBParser parseMethod: 'foo1: anObject foo1 := anObject')
]

{ #category : #testing }
RBAddVariableAccessorTransformationTest >> testTransform [

	| transformation class |
	transformation := (RBAddVariableAccessorTransformation
							instanceVariable: 'instVar'
							class: self changeMock name)
							transform.
	
	self assert: transformation model changes changes size equals: 2.
	
	class := transformation model classNamed: self changeMock name asSymbol.
	self assert: (class parseTreeFor: #instVar) = (RBParser parseMethod: 'instVar ^instVar').
	self assert: (class parseTreeFor: #instVar:) = (RBParser parseMethod: 'instVar: anObject instVar := anObject')
]
