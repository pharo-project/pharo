Class {
	#name : 'RBExtractMethodToComponentParametrizedTest',
	#superclass : 'RBAbstractRefactoringTest',
	#category : 'Refactoring-Transformations-Tests-SingleParametrized',
	#package : 'Refactoring-Transformations-Tests',
	#tag : 'SingleParametrized'
}

{ #category : 'tests' }
RBExtractMethodToComponentParametrizedTest class >> testParameters [
	^ ParametrizedTestMatrix new
		addCase: { #rbClass -> RBExtractMethodToComponentRefactoring };
		yourself
]

{ #category : 'accessing' }
RBExtractMethodToComponentParametrizedTest >> constructor [
	^ #extract:from:in:
]

{ #category : 'tests' }
RBExtractMethodToComponentParametrizedTest >> testExtractMethodAtEndOfMethodThatNeedsReturn [
	| refactoring class selectorsSize |
	refactoring := self createRefactoringWithArguments: { (52 to: 133) . #openEditor . RBLintRuleTestData }.
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
RBExtractMethodToComponentParametrizedTest >> testFailureBadInterval [
	self shouldFail: (self createRefactoringWithArguments:
		{ (24 to: 30) . #testMethod . RBClassDataForRefactoringTest }).
	self shouldFail: (self createRefactoringWithArguments:
		{ (80 to: 147) . #subclassOf:overrides: . RBBasicLintRuleTestData class })
]

{ #category : 'failure tests' }
RBExtractMethodToComponentParametrizedTest >> testFailureExtract [
	self shouldFail: (self createRefactoringWithArguments:
		{(80 to: 269) . #subclassOf:overrides: . RBBasicLintRuleTestData class }).
	self shouldFail: (self createRefactoringWithArguments:
		{ (53 to: 56) . #subclassOf:overrides: . RBBasicLintRuleTestData class }).
	self shouldFail: (self createRefactoringWithArguments:
		{ (77 to: 222) . #subclassResponsibilityNotDefined . RBBasicLintRuleTestData class })
]

{ #category : 'failure tests' }
RBExtractMethodToComponentParametrizedTest >> testFailureNonExistantSelector [
	self shouldFail: (self createRefactoringWithArguments:
		{ (10 to: 20) . #checkClass1: . RBBasicLintRuleTestData })
]

{ #category : 'tests' }
RBExtractMethodToComponentParametrizedTest >> testMoveWithoutSelfReference [
	| refactoring class selectorsSize |
	refactoring := self createRefactoringWithArguments: { (118 to: 285) . #copyDictionary: . RBReadBeforeWrittenTester }.
	self setupMethodNameFor: refactoring toReturn: #copyWithAssociations.
	self setupVariableToMoveToFor: refactoring toReturn: 'aDictionary'.
	self setupVariableTypesFor: refactoring toReturn: (Array with: (refactoring model classNamed: #Dictionary)).
	class := refactoring model classNamed: #RBReadBeforeWrittenTester.
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
