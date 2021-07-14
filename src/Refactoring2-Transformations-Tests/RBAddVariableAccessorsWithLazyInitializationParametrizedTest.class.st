Class {
	#name : #RBAddVariableAccessorsWithLazyInitializationParametrizedTest,
	#superclass : #RBAbstractRefactoringTest,
	#category : #'Refactoring2-Transformations-Tests-Parametrized'
}

{ #category : #tests }
RBAddVariableAccessorsWithLazyInitializationParametrizedTest class >> testParameters [
	^ ParametrizedTestMatrix new
		addCase: { #rbClass -> RBCreateAccessorsWithLazyInitializationForVariableRefactoring };
		yourself
]

{ #category : #running }
RBAddVariableAccessorsWithLazyInitializationParametrizedTest >> setUp [
	super setUp.
	model := self abstractVariableTestData.
]

{ #category : #'failure tests' }
RBAddVariableAccessorsWithLazyInitializationParametrizedTest >> testBadInitializationCode [
	self shouldFail: ( rbClass 
		variable: 'Foo1' 
		class: RBLintRuleTestData 
		classVariable: true 
		defaultValue: 'foo:')
]

{ #category : #'failure tests' }
RBAddVariableAccessorsWithLazyInitializationParametrizedTest >> testBadInitializationCode1 [
	self shouldFail: (rbClass 
		variable: 'Foo1' 
		class: RBLintRuleTestData 
		classVariable: true 
		defaultValue: '''string')
]

{ #category : #tests }
RBAddVariableAccessorsWithLazyInitializationParametrizedTest >> testExistingClassVariableAccessors [
	| refactoring class |
	refactoring := rbClass 
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
RBAddVariableAccessorsWithLazyInitializationParametrizedTest >> testExistingInstanceVariableAccessors [
	| refactoring class |
	refactoring := rbClass 
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
RBAddVariableAccessorsWithLazyInitializationParametrizedTest >> testNewClassVariableAccessors [
	| ref class |
	ref := rbClass 
		variable: 'Foo1' 
		class: RBLintRuleTestData 
		classVariable: true 
		defaultValue: '''someString'''.
	self executeRefactoring: ref.
	class := ref model metaclassNamed: #RBLintRuleTestData.
	self denyEmpty: ref changes changes.
	self assert: ref setterMethod identicalTo: #foo1:.
	self assert: ref getterMethod identicalTo: #foo1.
	self assert: (class parseTreeFor: #foo1) equals: (self parseMethod: 'foo1 ^Foo1 ifNil: [ Foo1 := ''someString'' ]').
	self assert: (class parseTreeFor: #foo1:) equals: (self parseMethod: 'foo1: anObject ^ Foo1 := anObject')
]

{ #category : #tests }
RBAddVariableAccessorsWithLazyInitializationParametrizedTest >> testNewInstanceVariableAccessors [
	| ref class |
	ref := rbClass 
		variable: 'foo1' 
		class: RBLintRuleTestData 
		classVariable: false 
		defaultValue: '123'.
	self executeRefactoring: ref.
	class := ref model classNamed: #RBLintRuleTestData.
	self denyEmpty: ref changes changes.
	self assert: ref setterMethod identicalTo: #foo1:.
	self assert: ref getterMethod identicalTo: #foo1.
	self assert: (class parseTreeFor: #foo1) equals: (self parseMethod: 'foo1 ^foo1 ifNil: [foo1 := 123]').
	self assert: (class parseTreeFor: #foo1:) equals: (self parseMethod: 'foo1: anObject foo1 := anObject')
]

{ #category : #'failure tests' }
RBAddVariableAccessorsWithLazyInitializationParametrizedTest >> testNonExistantName [
	self
		shouldFail: (rbClass 
				variable: #Foo
				class: RBBasicLintRuleTestData
				classVariable: true);
		shouldFail: (rbClass 
				variable: 'foo'
				class: RBBasicLintRuleTestData
				classVariable: true)
]
