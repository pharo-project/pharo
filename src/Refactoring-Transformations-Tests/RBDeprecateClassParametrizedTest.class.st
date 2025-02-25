Class {
	#name : 'RBDeprecateClassParametrizedTest',
	#superclass : 'RBAbstractRefactoringTest',
	#category : 'Refactoring-Transformations-Tests-SingleParametrized',
	#package : 'Refactoring-Transformations-Tests',
	#tag : 'SingleParametrized'
}

{ #category : 'tests' }
RBDeprecateClassParametrizedTest class >> testParameters [
	^ ParametrizedTestMatrix new
		addCase: { #rbClass -> RBDeprecateClassRefactoring };
		yourself
]

{ #category : 'accessing' }
RBDeprecateClassParametrizedTest >> constructor [
	^ #deprecate:in:
]

{ #category : 'failure tests' }
RBDeprecateClassParametrizedTest >> testFailureBadName [
	self shouldFail: (self createRefactoringWithArguments:
		{ 'RBLintRuleTestData' . self objectClassVariable }).
	self shouldFail: (self createRefactoringWithArguments:
		{ 'RBLintRuleTestData' . #'Ob ject' })
]

{ #category : 'failure tests' }
RBDeprecateClassParametrizedTest >> testFailureMetaClassFailure [

	self shouldFail: (self createRefactoringWithArguments: {
				 self class class name.
				 #Foo })
]

{ #category : 'tests' }
RBDeprecateClassParametrizedTest >> testModelRenameClass [

	| refactoring replacement deprecated user |
	model
		defineClass: [ :aBuilder |
			aBuilder
				superclass: Object;
				name: #ClassDeprecatorTestingClassReplacement;
				package: #'Refactory-Test data' ];
		defineClass: [ :aBuilder |
			aBuilder
				superclassName: #ClassDeprecatorTestingClassReplacement;
				name: #ClassDeprecatorTestingClassDeprecated;
				package: #'Refactory-Test data' ];
		defineClass: [ :aBuilder |
			aBuilder
				superclassName: #ClassDeprecatorTestingClassDeprecated;
				name: #ClassDeprecatorTestingClassDeprecatedUser;
				package: #'Refactory-Test data' ].

	replacement := model classNamed: #ClassDeprecatorTestingClassReplacement.
	deprecated := model classNamed: #ClassDeprecatorTestingClassDeprecated.
	user := model classNamed: #ClassDeprecatorTestingClassDeprecatedUser.

	user
		compile: 'foo	^ClassDeprecatorTestingClassDeprecated' classified: #( #accessing );
		compile: 'objectName ^#(ClassDeprecatorTestingClassDeprecated)' classified: #( #accessing ).
	refactoring := self createRefactoringWithModel: model andArguments: {
			               deprecated.
			               #ClassDeprecatorTestingClassReplacement }.

	"because of RB limitations we cannot detect extensions in the test properly so we skip this operation"
	refactoring shouldCopyExtensions: false.
	refactoring shouldRemoveExtensions: false.

	self executeRefactoring: refactoring.
	self assert: (model includesClassNamed: #ClassDeprecatorTestingClassReplacement).
	self assert: (model includesClassNamed: #ClassDeprecatorTestingClassDeprecated).
	self assert: (model includesClassNamed: #ClassDeprecatorTestingClassDeprecatedUser).

	self assert: (model classNamed: #ClassDeprecatorTestingClassReplacement) equals: replacement.
	self assert: (model classNamed: #ClassDeprecatorTestingClassDeprecated) equals: deprecated.
	self assert: (model classNamed: #ClassDeprecatorTestingClassDeprecatedUser) equals: user.

	self assert: (user parseTreeForSelector: #foo) equals: (self parseMethod: 'foo ^ClassDeprecatorTestingClassReplacement').

	self assert: (user parseTreeForSelector: #objectName) equals: (self parseMethod: 'objectName ^#(ClassDeprecatorTestingClassReplacement)').

	self assert: user superclass name equals: #ClassDeprecatorTestingClassReplacement
]
