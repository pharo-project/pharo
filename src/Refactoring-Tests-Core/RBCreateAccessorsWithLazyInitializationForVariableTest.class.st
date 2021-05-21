Class {
	#name : #RBCreateAccessorsWithLazyInitializationForVariableTest,
	#superclass : #RBRefactoringTest,
	#category : #'Refactoring-Tests-Core-Refactorings'
}

{ #category : #running }
RBCreateAccessorsWithLazyInitializationForVariableTest >> setUp [
	super setUp.
	model := self abstractVariableTestData.
]

{ #category : #'failure tests' }
RBCreateAccessorsWithLazyInitializationForVariableTest >> testBadInitializationCode [
	self shouldFail: ( RBCreateAccessorsWithLazyInitializationForVariableRefactoring 
		variable: 'Foo1' 
		class: RBLintRuleTestData 
		classVariable: true 
		defaultValue: 'foo:')
]

{ #category : #'failure tests' }
RBCreateAccessorsWithLazyInitializationForVariableTest >> testBadInitializationCode1 [
	self shouldFail: (RBCreateAccessorsWithLazyInitializationForVariableRefactoring 
		variable: 'Foo1' 
		class: RBLintRuleTestData 
		classVariable: true 
		defaultValue: '''string')
]

{ #category : #tests }
RBCreateAccessorsWithLazyInitializationForVariableTest >> testExistingClassVariableAccessors [
	| refactoring class |
	refactoring := RBCreateAccessorsWithLazyInitializationForVariableRefactoring 
							variable: 'Name1' 
							class: RBLintRuleTestData 
							classVariable: true.
	class := refactoring model classNamed: #RBLintRuleTestData.
	self assert: (class classSide parseTreeFor: #name1) 
		equals: (self parseMethod: 'name1 ^Name1').
	self executeRefactoring: refactoring.
	self assert: refactoring getterMethod identicalTo: #name1.
	self assert: (class classSide parseTreeFor: #name1) 
		equals: (self parseMethod: 'name1 ^Name1 ifNil: [Name1 := nil]').
]

{ #category : #tests }
RBCreateAccessorsWithLazyInitializationForVariableTest >> testExistingInstanceVariableAccessors [
	| refactoring class |
	refactoring := RBCreateAccessorsWithLazyInitializationForVariableRefactoring 
							variable: 'name' 
							class: RBLintRuleTestData 
							classVariable: false.
	class := refactoring model classNamed: #RBLintRuleTestData.
	self assert: (class parseTreeFor: #name) 
		equals: (self parseMethod: 'name ^name').
	self executeRefactoring: refactoring.
	self assert: refactoring getterMethod identicalTo: #name.
	self assert: (class parseTreeFor: #name) 
		equals: (self parseMethod: 'name ^name ifNil: [name := nil]').
]

{ #category : #tests }
RBCreateAccessorsWithLazyInitializationForVariableTest >> testNewClassVariableAccessors [
	| ref class |
	ref := RBCreateAccessorsWithLazyInitializationForVariableRefactoring variable: 'Foo1' class: RBLintRuleTestData classVariable: true defaultValue: '''someString'''.
	self executeRefactoring: ref.
	class := ref model metaclassNamed: #RBLintRuleTestData.
	self denyEmpty: ref changes changes.
	self assert: ref setterMethod identicalTo: #foo1:.
	self assert: ref getterMethod identicalTo: #foo1.
	self assert: (class parseTreeFor: #foo1) equals: (self parseMethod: 'foo1 ^Foo1 ifNil: [ Foo1 := ''someString'' ]').
	self assert: (class parseTreeFor: #foo1:) equals: (self parseMethod: 'foo1: anObject ^ Foo1 := anObject')
]

{ #category : #tests }
RBCreateAccessorsWithLazyInitializationForVariableTest >> testNewInstanceVariableAccessors [
	| ref class |
	ref := RBCreateAccessorsWithLazyInitializationForVariableRefactoring variable: 'foo1' class: RBLintRuleTestData classVariable: false defaultValue: '123'.
	self executeRefactoring: ref.
	class := ref model classNamed: #RBLintRuleTestData.
	self denyEmpty: ref changes changes.
	self assert: ref setterMethod identicalTo: #foo1:.
	self assert: ref getterMethod identicalTo: #foo1.
	self assert: (class parseTreeFor: #foo1) equals: (self parseMethod: 'foo1 ^foo1 ifNil: [foo1 := 123]').
	self assert: (class parseTreeFor: #foo1:) equals: (self parseMethod: 'foo1: anObject foo1 := anObject')
]

{ #category : #'failure tests' }
RBCreateAccessorsWithLazyInitializationForVariableTest >> testNonExistantName [
	self
		shouldFail: (RBCreateAccessorsWithLazyInitializationForVariableRefactoring 
				variable: #Foo
				class: RBBasicLintRuleTestData
				classVariable: true);
		shouldFail: (RBCreateAccessorsWithLazyInitializationForVariableRefactoring 
				variable: 'foo'
				class: RBBasicLintRuleTestData
				classVariable: true)
]
