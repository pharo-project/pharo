Class {
	#name : 'RBPushDownMethodParametrizedTest',
	#superclass : 'RBAbstractRefactoringTest',
	#category : 'Refactoring-Transformations-Tests-Parametrized',
	#package : 'Refactoring-Transformations-Tests',
	#tag : 'Parametrized'
}

{ #category : 'tests' }
RBPushDownMethodParametrizedTest class >> testParameters [
	^ ParametrizedTestMatrix new
		addCase: { #rbClass -> RePushDownMethodRefactoring };
		addCase: { #rbClass -> RBPushDownMethodTransformation };
		yourself
]

{ #category : 'accessing' }
RBPushDownMethodParametrizedTest >> constructor [
	^ #pushDown:from:
]

{ #category : 'failure tests' }
RBPushDownMethodParametrizedTest >> testFailurePushDownNonExistantMenu [

	| refactoring |
	refactoring := self createRefactoringWithArguments: {
			               #( #someMethodThatDoesNotExist ).
			               'RBLintRuleTestData' }.
	self shouldFail: refactoring
]

{ #category : 'tests' }
RBPushDownMethodParametrizedTest >> testPushDownMethod [

	| refactoring class |
	refactoring := self createRefactoringWithArguments: {
			               #( #name: ).
			               'RBLintRuleTestData' }.
	self proceedThroughWarning: [ self executeRefactoring: refactoring ].
	class := refactoring model classNamed: #RBLintRuleTestData.
	self deny: (class directlyDefinesMethod: #name:).
	class subclasses do: [ :each |
		self
			assert: (each parseTreeForSelector: #name:)
			equals: (self parseMethod: 'name: aString name := aString') ]
]

{ #category : 'tests' }
RBPushDownMethodParametrizedTest >> testPushDownMethodThatReferencesPoolDictionary [

	| refactoring class parseTree |
	parseTree := RBLintRuleTestData parseTreeForSelector: #junk.
	refactoring := self createRefactoringWithArguments: {
			               #( #junk ).
			               'RBLintRuleTestData' }.
	self proceedThroughWarning: [ self executeRefactoring: refactoring ].
	class := refactoring model classNamed: #RBLintRuleTestData.
	self deny: (class directlyDefinesMethod: #junk).
	class subclasses do: [ :each |
		self assert: (each parseTreeForSelector: #junk) equals: parseTree.
		self assert: (each definesPoolDictionary: 'RBSharedPoolForTestData' asSymbol) ]
]

{ #category : 'tests' }
RBPushDownMethodParametrizedTest >> testPushDownMethodWithAllSubclassesDefinesSelectorRemovesMethod [

	| refactoring class |
	refactoring := self createRefactoringWithArguments: {
			               #( #isEmpty ).
			               'RBLintRuleTestData' }.
	class := refactoring model classNamed: #RBLintRuleTestData.
	self assert: (class directlyDefinesMethod: #isEmpty).
	self proceedThroughWarning: [ self executeRefactoring: refactoring ].
	self deny: (class directlyDefinesMethod: #isEmpty)
]

{ #category : 'tests' }
RBPushDownMethodParametrizedTest >> testPushDownMethodWithMethodThatRefersToSelector [

	| refactoring |
	refactoring := self createRefactoringWithArguments: {
			               #( #problemCount ).
			               'RBLintRuleTestData' }.
	(refactoring isKindOf: RePushDownMethodRefactoring)
		ifTrue: [ self shouldWarn: refactoring ]
		ifFalse: [ self shouldntWarn: refactoring ]
]

{ #category : 'tests' }
RBPushDownMethodParametrizedTest >> testPushDownMethodWithSubclassOfSubclassThatDefinesSelector [

	| refactoring class subclass |
	refactoring := self createRefactoringWithArguments: {
			               #( #resetResult ).
			               'RBLintRuleTestData' }.
	self proceedThroughWarning: [ self executeRefactoring: refactoring ].
	class := refactoring model classNamed: #RBLintRuleTestData.
	subclass := refactoring model classNamed: #RBTransformationRuleTestData.
	self assert: (subclass parseTreeForSelector: #resetResult) equals: (self parseMethod: 'resetResult
	builder := ReChangeManager changeFactory compositeRefactoryChange')
]

{ #category : 'tests' }
RBPushDownMethodParametrizedTest >> testPushDownMethodWithSubclassesThatDefinesSelector [

	| refactoring |
	refactoring := self createRefactoringWithArguments: {
			               #( #isEmpty ).
			               'RBLintRuleTestData' }.
	(refactoring isKindOf: RePushDownMethodRefactoring)
		ifTrue: [ self shouldWarn: refactoring ]
		ifFalse: [ self shouldntWarn: refactoring ]
]
