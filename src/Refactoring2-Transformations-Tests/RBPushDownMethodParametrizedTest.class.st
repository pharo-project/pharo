Class {
	#name : #RBPushDownMethodParametrizedTest,
	#superclass : #RBAbstractRefactoringTest,
	#category : #'Refactoring2-Transformations-Tests-Parametrized'
}

{ #category : #tests }
RBPushDownMethodParametrizedTest class >> testParameters [
	^ ParametrizedTestMatrix new
		addCase: { #rbClass -> RBPushDownMethodRefactoring };
		addCase: { #rbClass -> RBPushDownMethodTransformation };
		yourself
]

{ #category : #accessing }
RBPushDownMethodParametrizedTest >> constructor [
	^ #pushDown:from:
]

{ #category : #'failure tests' }
RBPushDownMethodParametrizedTest >> testFailurePushDownMethodOnNonAbstractClass [

	| refactoring |
	refactoring := self createRefactoringWithArguments: {
			               #( #isArray ).
			               Array }.
	self shouldFail: refactoring asRefactoring
]

{ #category : #'failure tests' }
RBPushDownMethodParametrizedTest >> testFailurePushDownMethodSubclassesReferToSelector [

	| refactoring |
	model defineClass:
		'Object << #Superclass package: #''Refactory-Test data'''.
	model defineClass:
		'Superclass << #Foo1 package: #''Refactory-Test data'''.
	model defineClass:
		'Superclass << #Foo2 package: #''Refactory-Test data'''.
	(model classNamed: #Superclass)
		compile: 'yourself ^1'
		classified: #( #accessing ).
	(model classNamed: #Foo1)
		compile: 'method1 ^super yourself'
		classified: #( #accessing ).

	refactoring := rbClass
		               model: model
		               pushDown: #( #yourself )
		               from: (model classNamed: #Superclass).
	self shouldFail: refactoring asRefactoring
]

{ #category : #'failure tests' }
RBPushDownMethodParametrizedTest >> testFailurePushDownNonExistantMenu [

	| refactoring |
	refactoring := self createRefactoringWithArguments: {
			               #( #someMethodThatDoesNotExist ).
			               RBLintRuleTestData }.
	self shouldFail: refactoring asRefactoring
]

{ #category : #tests }
RBPushDownMethodParametrizedTest >> testPushDownMethod [
	| refactoring class |
	refactoring := self createRefactoringWithArguments:
		{ #(#name: ) . RBLintRuleTestData }.
	self executeRefactoring: refactoring asRefactoring.
	class := refactoring model classNamed: #RBLintRuleTestData.
	self deny: (class directlyDefinesMethod: #name:).
	class subclasses do:
		[ :each |
		self assert: (each parseTreeForSelector: #name:)
			equals: (self parseMethod: 'name: aString name := aString') ]
]

{ #category : #tests }
RBPushDownMethodParametrizedTest >> testPushDownMethodThatReferencesPoolDictionary [
	| refactoring class parseTree |
	parseTree := RBLintRuleTestData parseTreeForSelector: #junk.
	refactoring := self createRefactoringWithArguments:
		{ #(#junk ) . RBLintRuleTestData }.
	self executeRefactoring: refactoring asRefactoring.
	class := refactoring model classNamed: #RBLintRuleTestData.
	self deny: (class directlyDefinesMethod: #junk).
	class subclasses do:
		[ :each |
		self assert: (each parseTreeForSelector: #junk) equals: parseTree.
		self assert: (each definesPoolDictionary: 'TextConstants' asSymbol) ]
]
