Class {
	#name : #RBExtractMethodToComponentTest,
	#superclass : #RBRefactoringTest,
	#category : #'Refactoring-Tests-Core'
}

{ #category : #'failure tests' }
RBExtractMethodToComponentTest >> testBadInterval [
	self
		shouldFail: (RBExtractMethodToComponentRefactoring 
				extract: (self 
						convertInterval: (24 to: 30)
						for: (RBRefactoryTestDataApp sourceCodeAt: #testMethod))
				from: #testMethod
				in: RBRefactoryTestDataApp);
		shouldFail: (RBExtractMethodToComponentRefactoring 
				extract: (self 
						convertInterval: (80 to: 147)
						for: (RBBasicLintRuleTestData class sourceCodeAt: #subclassOf:overrides:))
				from: #subclassOf:overrides:
				in: RBBasicLintRuleTestData class)
]

{ #category : #'failure tests' }
RBExtractMethodToComponentTest >> testExtractFailure [
	self
		shouldFail: (RBExtractMethodToComponentRefactoring 
				extract: (self 
						convertInterval: (80 to: 269)
						for: (RBBasicLintRuleTestData class sourceCodeAt: #subclassOf:overrides:))
				from: #subclassOf:overrides:
				in: RBBasicLintRuleTestData class);
		shouldFail: (RBExtractMethodToComponentRefactoring 
				extract: (self 
						convertInterval: (53 to: 56)
						for: (RBBasicLintRuleTestData class sourceCodeAt: #subclassOf:overrides:))
				from: #subclassOf:overrides:
				in: RBBasicLintRuleTestData class);
		shouldFail: (RBExtractMethodToComponentRefactoring 
				extract: (self 
						convertInterval: (77 to: 222)
						for: (RBBasicLintRuleTestData class sourceCodeAt: #subclassResponsibilityNotDefined))
				from: #subclassResponsibilityNotDefined
				in: RBBasicLintRuleTestData class)
]

{ #category : #tests }
RBExtractMethodToComponentTest >> testExtractMethodAtEndOfMethodThatNeedsReturn [
	| refactoring class selectorsSize |
	refactoring := RBExtractMethodToComponentRefactoring 
		extract: (self 
				convertInterval: (52 to: 133)
				for: (RBLintRuleTestData sourceCodeAt: #openEditor))
		from: #openEditor
		in: RBLintRuleTestData.
	self 
		setupMethodNameFor: refactoring
		toReturn: #foo:.
	self 
		setupSelfArgumentNameFor: refactoring
		toReturn: 'asdf'.
	self 
		setupVariableToMoveToFor: refactoring
		toReturn: 'rules'.
	self 
		setupVariableTypesFor: refactoring
		toReturn: (Array with: (refactoring model classNamed: #Collection)).
	class := refactoring model classNamed: #RBLintRuleTestData.
	selectorsSize := class selectors size.
	self proceedThroughWarning: [ self executeRefactoring: refactoring ].
	self assert: (class parseTreeFor: #openEditor) = (self parseMethod: 'openEditor
								| rules |
								rules := self failedRules.
								^rules foo: self').
	self assert: ((refactoring model classNamed: #Collection) parseTreeFor: #foo:) = (self parseMethod: 'foo: asdf
								self isEmpty ifTrue: [^asdf].
								self size == 1 ifTrue: [^self first viewResults].
								^asdf').
	self assert: class selectors size = selectorsSize
]

{ #category : #tests }
RBExtractMethodToComponentTest >> testMoveWithoutSelfReference [
	| refactoring class selectorsSize |
	refactoring := RBExtractMethodToComponentRefactoring 
				extract: (self convertInterval: (118 to: 286)
						for: (RBReadBeforeWrittenTester sourceCodeAt: #copyDictionary:))
				from: #copyDictionary:
				in: RBReadBeforeWrittenTester.
	self setupMethodNameFor: refactoring toReturn: #copyWithAssociations.
	self setupVariableToMoveToFor: refactoring toReturn: 'aDictionary'.
	self setupVariableTypesFor: refactoring
		toReturn: (Array with: (refactoring model classNamed: #Dictionary)).
	class := refactoring model classNamed: #RBReadBeforeWrittenTester.
	selectorsSize := class selectors size.
	self executeRefactoring: refactoring.
	self 
		assert: (class parseTreeFor: #copyDictionary:) = (self 
						parseMethod: 'copyDictionary: aDictionary ^aDictionary copyWithAssociations').
	self 
		assert: ((refactoring model classNamed: #Dictionary) 
				parseTreeFor: #copyWithAssociations) = (self 
							parseMethod: 'copyWithAssociations 
							| newDictionary |
							newDictionary := Dictionary new: self size.
							self
								keysAndValuesDo: [:key :value | newDictionary at: key put: value].
							^newDictionary').
	self assert: class selectors size = selectorsSize
]

{ #category : #'failure tests' }
RBExtractMethodToComponentTest >> testNonExistantSelector [
	self shouldFail: (RBExtractMethodToComponentRefactoring 
			extract: (10 to: 20)
			from: #checkClass1:
			in: RBBasicLintRuleTestData)
]
