Class {
	#name : #RBPushDownClassVariableParametrizedTest,
	#superclass : #RBWithDifferentConstructorsParametrizedTest,
	#category : #'Refactoring2-Transformations-Tests-Parametrized'
}

{ #category : #tests }
RBPushDownClassVariableParametrizedTest class >> testParameters [
	^ ParametrizedTestMatrix new
		addCase: { #rbClass -> RBPushDownClassVariableRefactoring .
					  #constructor -> #variable:class: };
		addCase: { #rbClass -> RBPushDownVariableTransformation .
					  #constructor -> #classVariable:class: };
		yourself
]

{ #category : #'failure tests' }
RBPushDownClassVariableParametrizedTest >> testFailureModelNonExistantName [

	model defineClass:
		'Object << #SomeClass package: #''Refactory-Test data'''.
	self shouldFail: (self
			 createRefactoringWithModel: model
			 andArguments: { #Foo. #SomeClass })
]

{ #category : #'failure tests' }
RBPushDownClassVariableParametrizedTest >> testFailureModelPushDownToMultipleSubclassesFailure [

	model defineClass: 'Object << #SomeClass
		sharedVariables: { #Foo } ;
		package: #''Refactory-Test data'''.
	model defineClass: 'SomeClass << #Subclass
		package: #''Refactory-Test data'''.
	(model classNamed: #Subclass)
		compile: 'foo ^Foo'
		classified: #( #accessing ).
	model defineClass: 'SomeClass << #AnotherSubclass
		package: #''Refactory-Test data'''.
	(model metaclassNamed: #AnotherSubclass)
		compile: 'bar ^Foo'
		classified: #( #accessing ).
	self shouldFail: (self
			 createRefactoringWithModel: model
			 andArguments: { #Foo. #SomeClass })
]

{ #category : #'failure tests' }
RBPushDownClassVariableParametrizedTest >> testFailureNonExistantName [

	self shouldFail: (self createRefactoringWithArguments:
			 { #Foo. #RBBasicLintRuleTestData })
]

{ #category : #'failure tests' }
RBPushDownClassVariableParametrizedTest >> testModelNonExistantName [
	model defineClass: 'Object << #SomeClass package: #''Refactory-Test data'''.
	self shouldFail: (self createRefactoringWithModel: model
		andArguments: { #Foo . #SomeClass })
]

{ #category : #'failure tests' }
RBPushDownClassVariableParametrizedTest >> testModelPushDownToMultipleSubclassesFailure [
	model defineClass: 'Object << #SomeClass
		sharedVariables: { #Foo } ;
		package: #''Refactory-Test data'''.
	model defineClass: 'SomeClass << #Subclass
		package: #''Refactory-Test data'''.
	(model classNamed: #Subclass) compile: 'foo ^Foo' classified: #(#accessing).
	model defineClass: 'SomeClass << #AnotherSubclass
		package: #''Refactory-Test data'''.
	(model metaclassNamed: #AnotherSubclass) compile: 'bar ^Foo'
		classified: #(#accessing).
	self shouldFail: (self createRefactoringWithModel: model
		andArguments: { #Foo . #SomeClass })
]

{ #category : #tests }
RBPushDownClassVariableParametrizedTest >> testModelPushDownVariable [
	| class |
	model defineClass: 'Object << #SomeClass
		sharedVariables: { #Foo };
		package: #''Refactory-Test data'''.
	model defineClass: 'SomeClass << #Subclass
		package: #''Refactory-Test data'''.
	class := model classNamed: #Subclass.
	class compile: 'foo ^Foo' classified: #(#accessing).
	self executeRefactoring: (self createRefactoringWithModel: model
		andArguments: { #Foo . #SomeClass}).
	self deny: (class superclass directlyDefinesClassVariable: #Foo).
	self assert: (class directlyDefinesClassVariable: #Foo)
]

{ #category : #tests }
RBPushDownClassVariableParametrizedTest >> testModelPushDownVariableToClassDownTwoLevels [
	| class |
	model defineClass: 'Object << #SomeClass
		sharedVariables: { #Foo };
		package: #''Refactory-Test data'''.
	model defineClass: 'SomeClass << #Subclass
		package: #''Refactory-Test data'''.
	model defineClass: 'Subclass << #AnotherSubclass
		package: #''Refactory-Test data'''.
	class := model metaclassNamed: #AnotherSubclass.
	class compile: 'bar ^Foo' classified: #(#accessing ).
	class := class instanceSide.

	self executeRefactoring: (self createRefactoringWithModel: model
		andArguments: {#Foo . #SomeClass}).

	self deny: (class superclass superclass directlyDefinesClassVariable: #Foo).
	self deny: (class superclass directlyDefinesClassVariable: #Foo).
	self assert: (class directlyDefinesClassVariable: #Foo)
]

{ #category : #tests }
RBPushDownClassVariableParametrizedTest >> testModelPushDownVariableToMultipleClassesInSameHierarchy [
	| class |
	model defineClass: 'Object << #SomeClass
		sharedVariables: { #Foo } ;
		package: #''Refactory-Test data'''.
	model defineClass: 'SomeClass << #Subclass package: #''Refactory-Test data'''.
	class := model classNamed: #Subclass.
	class compile: 'foo ^Foo' classified: #(#accessing).
	model defineClass: 'Subclass << #AnotherSubclass  package: #''Refactory-Test data'''.
	(model metaclassNamed: #AnotherSubclass) compile: 'bar ^Foo'
		classified: #(#accessing).
	self executeRefactoring: (self createRefactoringWithModel: model
		andArguments: {#Foo . #SomeClass}).
	self deny: (class superclass directlyDefinesClassVariable: #Foo).
	self assert: (class directlyDefinesClassVariable: #Foo)
]

{ #category : #tests }
RBPushDownClassVariableParametrizedTest >> testModelRemoveUnusedVariable [
	model defineClass: 'Object << #SomeClass
		sharedVariables: { #Foo };
		package: #''Refactory-Test data'''.
	self
		assert: ((model classNamed: #SomeClass) directlyDefinesVariable: #Foo).
	model defineClass: 'SomeClass << #Subclass package: #''Refactory-Test data'''.
	self executeRefactoring: (self createRefactoringWithModel: model
		andArguments: { #Foo . #SomeClass }).
	self deny: ((model classNamed: #SomeClass) directlyDefinesVariable: #Foo).
	self deny: ((model classNamed: #Subclass) directlyDefinesVariable: #Foo)
]

{ #category : #'failure tests' }
RBPushDownClassVariableParametrizedTest >> testNonExistantName [
	self shouldFail:
		(self createRefactoringWithArguments: { #Foo . #RBBasicLintRuleTestData })
]

{ #category : #tests }
RBPushDownClassVariableParametrizedTest >> testPushDownClassVariable [
	| refactoring class |
	refactoring := self createRefactoringWithArguments:
		{ #Foo1 . #RBLintRuleTestData }.
	class := (refactoring model classNamed: #RBLintRuleTestData).

	self assert: (class directlyDefinesClassVariable: #Foo1).

	self executeRefactoring: refactoring.
		class withAllSubclasses
			do: [ :each | self deny: (each directlyDefinesClassVariable: #Foo1) ]
]
