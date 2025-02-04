Class {
	#name : 'RBExtractMethodToComponentRefactoringTest',
	#superclass : 'RBAbstractRefactoringTest',
	#category : 'Refactoring-Transformations-Tests-SingleParametrized',
	#package : 'Refactoring-Transformations-Tests',
	#tag : 'SingleParametrized'
}

{ #category : 'tests' }
RBExtractMethodToComponentRefactoringTest >> testExtractMethodAtEndOfMethodThatNeedsReturn [
	| refactoring class selectorsSize |
	refactoring := RBExtractMethodToComponentRefactoring extract: (52 to: 133) from: #openEditor in: RBLintRuleTestData.
	self setupMethodNameFor: refactoring toReturn: #foo:.
	self setupSelfArgumentNameFor: refactoring toReturn: 'asdf'.
	self setupVariableToMoveToFor: refactoring toReturn: 'rules'.
	self setupVariableTypesFor: refactoring toReturn: (Array with: (refactoring model classNamed: #Collection)).
	class := refactoring model classNamed: #RBLintRuleTestData.
	selectorsSize := class selectors size.
	self proceedThroughWarning: [ self executeRefactoring: refactoring ].

	self
		assert: (class parseTreeForSelector: #openEditor)
		equals:
			(self
				parseMethod:
					'openEditor
								| rules |
								rules := self failedRules.
								^rules foo: self').
	self
		assert: ((refactoring model classNamed: #Collection) parseTreeForSelector: #foo:)
		equals:
			(self
				parseMethod:
					'foo: asdf
								self isEmpty ifTrue: [^asdf].
								self size == 1 ifTrue: [^self first viewResults].
								^asdf').
	self assert: class selectors size equals: selectorsSize
]

{ #category : 'failure tests' }
RBExtractMethodToComponentRefactoringTest >> testFailureBadInterval [
	self shouldFail: (RBExtractMethodToComponentRefactoring
		extract: (24 to: 30) from: #testMethod in: RBClassDataForRefactoringTest).
	self shouldFail: (RBExtractMethodToComponentRefactoring
		extract: (80 to: 147) from: #subclassOf:overrides: in: RBBasicLintRuleTestData class)
]

{ #category : 'failure tests' }
RBExtractMethodToComponentRefactoringTest >> testFailureExtract [
	self shouldFail: (RBExtractMethodToComponentRefactoring
		extract: (80 to: 269) from: #subclassOf:overrides: in: RBBasicLintRuleTestData class).
	self shouldFail: (RBExtractMethodToComponentRefactoring
		extract: (53 to: 56) from: #subclassOf:overrides: in: RBBasicLintRuleTestData class).
	self shouldFail: (RBExtractMethodToComponentRefactoring
		extract: (77 to: 222) from: #subclassResponsibilityNotDefined in: RBBasicLintRuleTestData class)
]

{ #category : 'failure tests' }
RBExtractMethodToComponentRefactoringTest >> testFailureNonExistantSelector [
	self shouldFail: (RBExtractMethodToComponentRefactoring
		extract: (10 to: 20) from: #checkClass1: in: RBBasicLintRuleTestData)
]

{ #category : 'tests' }
RBExtractMethodToComponentRefactoringTest >> testMoveWithoutSelfReference [
	| refactoring class selectorsSize |
	refactoring := RBExtractMethodToComponentRefactoring extract: (118 to: 285) from: #copyDictionary: in: OCReadBeforeWrittenTester.
	self setupMethodNameFor: refactoring toReturn: #copyWithAssociations.
	self setupVariableToMoveToFor: refactoring toReturn: 'aDictionary'.
	self setupVariableTypesFor: refactoring toReturn: (Array with: (refactoring model classNamed: #Dictionary)).
	class := refactoring model classNamed: #OCReadBeforeWrittenTester.
	selectorsSize := class selectors size.
	self executeRefactoring: refactoring.

	self assert: (class parseTreeForSelector: #copyDictionary:) equals: (self parseMethod: 'copyDictionary: aDictionary ^aDictionary copyWithAssociations').
	self
		assert: ((refactoring model classNamed: #Dictionary) parseTreeForSelector: #copyWithAssociations)
		equals:
			(self
				parseMethod:
					'copyWithAssociations
							| newDictionary |
							newDictionary := Dictionary new: self size.
							self
								keysAndValuesDo: [:key :value | newDictionary at: key put: value].
							^newDictionary').
	self assert: class selectors size equals: selectorsSize
]
