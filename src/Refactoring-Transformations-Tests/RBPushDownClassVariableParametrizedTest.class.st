Class {
	#name : 'RBPushDownClassVariableParametrizedTest',
	#superclass : 'RBWithDifferentConstructorsParametrizedTest',
	#category : 'Refactoring-Transformations-Tests-Parametrized',
	#package : 'Refactoring-Transformations-Tests',
	#tag : 'Parametrized'
}

{ #category : 'building suites' }
RBPushDownClassVariableParametrizedTest class >> testParameters [
	^ ParametrizedTestMatrix new
		addCase: { #rbClass -> RBPushDownClassVariableRefactoring .
					  #constructor -> #variable:class: };
		addCase: { #rbClass -> RBPushDownVariableTransformation .
					  #constructor -> #classVariable:class: };
		yourself
]

{ #category : 'running' }
RBPushDownClassVariableParametrizedTest >> setUp [

	super setUp.
	model := self modelOnClasses: { Object . RBBasicLintRuleTestData . RBLintRuleTestData }
]

{ #category : 'failure tests' }
RBPushDownClassVariableParametrizedTest >> testFailureModelNonExistantName [

	model defineClass: [ :aBuilder |
		aBuilder
			superclass: Object;
			name: #RootOrigin;
			package: #'Refactory-Test data' ].
	self shouldFail: (self createRefactoringWithModel: model andArguments: { #UndefinedName . #RootOrigin })
]

{ #category : 'failure tests' }
RBPushDownClassVariableParametrizedTest >> testFailureNonExistantName [
	
	self shouldFail: (self createRefactoringWithModel: model andArguments:
			 { #NOTDefinedVariable. #RBBasicLintRuleTestData })
]

{ #category : 'tests' }
RBPushDownClassVariableParametrizedTest >> testModelPushDownVariable [

	| class |
	model defineClass: [ :aBuilder |
		aBuilder
			superclass: Object;
			name: #RootOrigin;
			sharedVariables: { #Foo };
			package: #'Refactory-Test data' ].
	model defineClass: [ :aBuilder |
		aBuilder
			superclassName: #RootOrigin;
			name: #SubRoot;
			package: #'Refactory-Test data' ].
	class := model classNamed: #SubRoot.
	"We define a user of the shared variable because the refactoring is trying to be smart."
	class compile: 'foo ^Foo' classified: #( #accessing ).
	self executeRefactoring: (self createRefactoringWithModel: model andArguments: { #Foo. #RootOrigin }).
	self deny: (class superclass directlyDefinesClassVariable: #Foo).
	self assert: (class directlyDefinesClassVariable: #Foo)
]

{ #category : 'tests' }
RBPushDownClassVariableParametrizedTest >> testModelPushDownVariableToClassDownTwoLevels [

	| class |
	model defineClass: [ :aBuilder |
		aBuilder
			superclass: Object;
			name: #RootOrigin;
			sharedVariables: { #Foo };
			package: #'Refactory-Test data' ].
	model defineClass: [ :aBuilder |
		aBuilder
			superclassName: #RootOrigin;
			name: #SubRoot;
			package: #'Refactory-Test data' ].
	model defineClass: [ :aBuilder |
		aBuilder
			superclassName: #SubRoot;
			name: #SubSubRoot;
			package: #'Refactory-Test data' ].

	"We define a user of the shared variable because the refactoring is trying to be smart."
	class := model metaclassNamed: #SubSubRoot.
	class compile: 'bar ^Foo' classified: #( #accessing ).


	class := class instanceSide.

	self executeRefactoring: (self createRefactoringWithModel: model andArguments: { #Foo. #RootOrigin }).

	self deny: (class superclass superclass directlyDefinesClassVariable: #Foo).
	self deny: (class superclass directlyDefinesClassVariable: #Foo).
	
	self assert: (class directlyDefinesClassVariable: #Foo)
]

{ #category : 'tests' }
RBPushDownClassVariableParametrizedTest >> testModelPushDownVariableToMultipleClassesInSameHierarchy [

	| class1 class2 |

	model
		defineClass: [ :aBuilder |
			aBuilder
				superclass: Object;
				name: #RootOrigin;
				sharedVariables: { #Foo };
				package: #'Refactory-Test data' ];
		defineClass: [ :aBuilder |
			aBuilder
				superclassName: #RootOrigin;
				name: #SubRoot;
				package: #'Refactory-Test data' ].
	class1 := model classNamed: #SubRoot.
	class1 compile: 'foo ^Foo' classified: #( #accessing ).
	model defineClass: [ :aBuilder |
		aBuilder
			superclassName: #RootOrigin;
			name: #SubRoot2;
			package: #'Refactory-Test data' ].
	class2 := model classNamed: #SubRoot2.
	(model metaclassNamed: #SubRoot2) compile: 'bar ^Foo' classified: #( #accessing ).
	self executeRefactoring: (self createRefactoringWithModel: model andArguments: { #Foo. #RootOrigin }).
	self deny: (class1 superclass directlyDefinesClassVariable: #Foo).
	self assert: (class2 directlyDefinesClassVariable: #Foo).
	self assert: (class1 directlyDefinesClassVariable: #Foo)
]

{ #category : 'tests' }
RBPushDownClassVariableParametrizedTest >> testModelRemoveUnusedVariable [

	model defineClass: [ :aBuilder |
		aBuilder
			superclass: Object;
			name: #SomeClass;
			sharedVariables: { #Foo };
			package: #'Refactory-Test data' ].
	self assert: ((model classNamed: #SomeClass) directlyDefinesVariable: #Foo).
	model defineClass: [ :aBuilder |
		aBuilder
			superclassName: #SomeClass;
			name: #Subclass;
			package: #'Refactory-Test data' ].
	self executeRefactoring: (self createRefactoringWithModel: model andArguments: { #Foo. #SomeClass }).
	self deny: ((model classNamed: #SomeClass) directlyDefinesVariable: #Foo).
	self deny: ((model classNamed: #Subclass) directlyDefinesVariable: #Foo)
]

{ #category : 'tests' }
RBPushDownClassVariableParametrizedTest >> testPushDownUnusedClassVariable [
	| refactoring class |
	refactoring := self createRefactoringWithModel: model andArguments:
		{ #Foo1 . #RBLintRuleTestData }.
		
	class := (refactoring model classNamed: #RBLintRuleTestData).
	self assert: (class directlyDefinesClassVariable: #Foo1).

	self executeRefactoring: refactoring.

	class withAllSubclasses
			do: [ :each | self deny: (each directlyDefinesClassVariable: #Foo1) ]
]
