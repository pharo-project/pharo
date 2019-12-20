Class {
	#name : #RBCreateAccessorsForVariableTest,
	#superclass : #RBRefactoringTest,
	#category : #'Refactoring-Tests-Core'
}

{ #category : #running }
RBCreateAccessorsForVariableTest >> setUp [
	super setUp.
	model := self abstractVariableTestData.
]

{ #category : #tests }
RBCreateAccessorsForVariableTest >> testExistingInstanceVariableAccessors [
	| ref |
	ref := RBCreateAccessorsForVariableRefactoring variable: 'name' class: RBLintRuleTestData classVariable: false.
	self executeRefactoring: ref.
	self assertEmpty: ref changes changes.
	self assert: ref setterMethod identicalTo: #name:.
	self assert: ref getterMethod identicalTo: #name
]

{ #category : #tests }
RBCreateAccessorsForVariableTest >> testNewClassVariableAccessors [
	| ref class |
	ref := RBCreateAccessorsForVariableRefactoring variable: 'Foo1' class: RBLintRuleTestData classVariable: true.
	self executeRefactoring: ref.
	class := ref model metaclassNamed: #RBLintRuleTestData.
	self denyEmpty: ref changes changes.
	self assert: ref setterMethod identicalTo: #foo1:.
	self assert: ref getterMethod identicalTo: #foo1.
	self assert: (class parseTreeFor: #foo1) equals: (self parseMethod: 'foo1 ^Foo1').
	self assert: (class parseTreeFor: #foo1:) equals: (self parseMethod: 'foo1: anObject Foo1 := anObject')
]

{ #category : #tests }
RBCreateAccessorsForVariableTest >> testNewInstanceVariableAccessors [
	| ref class |
	ref := RBCreateAccessorsForVariableRefactoring variable: 'foo1' class: RBLintRuleTestData classVariable: false.
	self executeRefactoring: ref.
	class := ref model classNamed: #RBLintRuleTestData.
	self denyEmpty: ref changes changes.
	self assert: ref setterMethod identicalTo: #foo1:.
	self assert: ref getterMethod identicalTo: #foo1.
	self assert: (class parseTreeFor: #foo1) equals: (self parseMethod: 'foo1 ^foo1').
	self assert: (class parseTreeFor: #foo1:) equals: (self parseMethod: 'foo1: anObject foo1 := anObject')
]

{ #category : #'failure tests' }
RBCreateAccessorsForVariableTest >> testNonExistantName [
	self
		shouldFail: (RBCreateAccessorsForVariableRefactoring 
				variable: #Foo
				class: RBBasicLintRuleTestData
				classVariable: true);
		shouldFail: (RBCreateAccessorsForVariableRefactoring 
				variable: 'foo'
				class: RBBasicLintRuleTestData
				classVariable: true)
]
