Class {
	#name : 'RBAddVariableAccessorsWithLazyInitializationParametrizedTest',
	#superclass : 'RBAbstractRefactoringTest',
	#category : 'Refactoring-Transformations-Tests-Parametrized',
	#package : 'Refactoring-Transformations-Tests',
	#tag : 'Parametrized'
}

{ #category : 'tests' }
RBAddVariableAccessorsWithLazyInitializationParametrizedTest class >> testParameters [
	^ ParametrizedTestMatrix new
		addCase: { #rbClass -> RBCreateLazyAccessorsForVariableTransformation };
		addCase: { #rbClass -> RBAddVariableAccessorWithLazyInitializationTransformation };
		yourself
]

{ #category : 'accessing' }
RBAddVariableAccessorsWithLazyInitializationParametrizedTest >> constructor [
	^ #variable:class:classVariable:defaultValue:
]

{ #category : 'running' }
RBAddVariableAccessorsWithLazyInitializationParametrizedTest >> setUp [
	super setUp.
	model := self rbModelForVariableTest
]

{ #category : 'tests' }
RBAddVariableAccessorsWithLazyInitializationParametrizedTest >> testExistingClassVariableAccessors [
	| refactoring class |

	refactoring := self createRefactoringWithArguments:
		{ 'Name1' . RBLintRuleTestData . true . nil }.
	class := refactoring model classNamed: #RBLintRuleTestData.
	self assert: (class classSide parseTreeForSelector: #name1)
		equals: (self parseMethod: 'name1 ^Name1').
	self executeRefactoring: refactoring.
	self assert: (class classSide parseTreeForSelector: #name1)
		equals: (self parseMethod: 'name1 ^Name1 ifNil: [Name1 := nil]')
]

{ #category : 'tests' }
RBAddVariableAccessorsWithLazyInitializationParametrizedTest >> testExistingInstanceVariableAccessors [
	| refactoring class |
	refactoring := self createRefactoringWithArguments:
		{ 'name' . RBLintRuleTestData . false . nil }.
	class := refactoring model classNamed: #RBLintRuleTestData.
	self assert: (class parseTreeForSelector: #name)
		equals: (self parseMethod: 'name ^name').
	self executeRefactoring: refactoring.
	self assert: (class parseTreeForSelector: #name)
		equals: (self parseMethod: 'name ^name ifNil: [name := nil]')
]

{ #category : 'failure tests' }
RBAddVariableAccessorsWithLazyInitializationParametrizedTest >> testFailureBadDefaultValueForNewArgument [

	self shouldFail: (self createRefactoringWithArguments: {
				 'Foo1'.
				 RBLintRuleTestData.
				 true.
				 'foo:' })
]

{ #category : 'failure tests' }
RBAddVariableAccessorsWithLazyInitializationParametrizedTest >> testFailureBadInitializationCode1 [

	self shouldFail: (self createRefactoringWithArguments: {
				 'Foo1'.
				 RBLintRuleTestData.
				 true.
				 '''string' })
]

{ #category : 'failure tests' }
RBAddVariableAccessorsWithLazyInitializationParametrizedTest >> testFailureNonExistantName [

	self
		shouldFail: (self createRefactoringWithArguments: {
						 #Foo.
						 RBBasicLintRuleTestData.
						 true.
						 nil });
		shouldFail: (self createRefactoringWithArguments: {
						 'foo'.
						 RBBasicLintRuleTestData.
						 true.
						 nil })
]

{ #category : 'tests' }
RBAddVariableAccessorsWithLazyInitializationParametrizedTest >> testNewClassVariableAccessors [
	| ref class |
	ref := self createRefactoringWithArguments:
		{'Foo1' . RBLintRuleTestData . true . '''someString'''}.
	self executeRefactoring: ref.
	class := ref model metaclassNamed: #RBLintRuleTestData.
	self denyEmpty: ref changes changes.
	self assert: (class parseTreeForSelector: #foo1) equals: (self parseMethod: 'foo1 ^Foo1 ifNil: [ Foo1 := ''someString'' ]').
	self assert: (((class parseTreeForSelector: #foo1:) = (self parseMethod: 'foo1: anObject ^ Foo1 := anObject'))
		or: [ (class parseTreeForSelector: #foo1:) = (self parseMethod: 'foo1: anObject Foo1 := anObject') ])
]

{ #category : 'tests' }
RBAddVariableAccessorsWithLazyInitializationParametrizedTest >> testNewInstanceVariableAccessors [
	| ref class |
	ref := self createRefactoringWithArguments:
		{'foo1' . RBLintRuleTestData . false . '123'}.
	self executeRefactoring: ref.
	class := ref model classNamed: #RBLintRuleTestData.
	self denyEmpty: ref changes changes.
	self assert: (class parseTreeForSelector: #foo1) equals: (self parseMethod: 'foo1 ^foo1 ifNil: [foo1 := 123]').
	self assert: (class parseTreeForSelector: #foo1:) equals: (self parseMethod: 'foo1: anObject foo1 := anObject')
]
