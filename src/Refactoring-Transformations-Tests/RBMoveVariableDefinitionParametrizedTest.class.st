Class {
	#name : 'RBMoveVariableDefinitionParametrizedTest',
	#superclass : 'RBAbstractRefactoringTest',
	#category : 'Refactoring-Transformations-Tests-SingleParametrized',
	#package : 'Refactoring-Transformations-Tests',
	#tag : 'SingleParametrized'
}

{ #category : 'building suites' }
RBMoveVariableDefinitionParametrizedTest class >> testParameters [
	^ ParametrizedTestMatrix new
		addCase: { #rbClass -> RBMoveVariableDefinitionToSmallestValidScopeRefactoring };
		yourself
]

{ #category : 'accessing' }
RBMoveVariableDefinitionParametrizedTest >> constructor [
	^ #bindTight:in:selector:
]

{ #category : 'failure tests' }
RBMoveVariableDefinitionParametrizedTest >> testFailureInvalidSelector [

	self shouldFail: (self createRefactoringWithArguments: {
				 (21 to: 24).
				 RBClassDataForRefactoringTest.
				 #caller3 })
]

{ #category : 'failure tests' }
RBMoveVariableDefinitionParametrizedTest >> testFailureNoMoveDefinition [

	self shouldFail: (self createRefactoringWithArguments: {
				 (21 to: 24).
				 RBClassDataForRefactoringTest.
				 #noMoveDefinition })
]

{ #category : 'failure tests' }
RBMoveVariableDefinitionParametrizedTest >> testFailureNonExistantName [

	self shouldFail: (self createRefactoringWithArguments: {
				 (1 to: 10).
				 RBLintRuleTestData.
				 #name1 }).
	self shouldFail: (self createRefactoringWithArguments: {
				 (44 to: 54).
				 RBLintRuleTestData.
				 #displayName }).
	self shouldFail: (self createRefactoringWithArguments: {
				 (16 to: 25).
				 RBLintRuleTestData.
				 #displayName })
]

{ #category : 'tests' }
RBMoveVariableDefinitionParametrizedTest >> testMoveDefinition [
	| refactoring |
	refactoring := self createRefactoringWithArguments:
		{ (19 to: 22) . RBClassDataForRefactoringTest . #moveDefinition }.
	self executeRefactoring: refactoring.

	self assert: ((refactoring model classNamed: #RBClassDataForRefactoringTest) parseTreeForSelector: #moveDefinition) equals: (self parseMethod: 'moveDefinition
								^(self collect:
										[:each |
										| temp |
										temp := each printString.
										temp , temp])
									select:
										[:each |
										| temp |
										temp := each size.
										temp odd]')
]

{ #category : 'tests' }
RBMoveVariableDefinitionParametrizedTest >> testMoveDefinitionIntoBlockThatIsAReceiverOfACascadedMessage [
	| refactoring |
	refactoring := self createRefactoringWithArguments:
		{ (47 to: 57) . RBClassDataForRefactoringTest . #referencesConditionFor: }.
	self executeRefactoring: refactoring.
	self assert: ((refactoring model classNamed: #RBClassDataForRefactoringTest) parseTreeForSelector: #referencesConditionFor:) equals: (self parseMethod: 'referencesConditionFor: aClass
								| environment  |
								^(RBCondition withBlock:
										[| association |association := Smalltalk globals associationAt: aClass name
													ifAbsent: [self refactoringError: ''Could not find class''].
										environment := (self environment referencesTo: association)
													| (self environment referencesTo: aClass name).
										environment isEmpty])
									errorMacro: aClass , '' is referenced.<n>Browse references?'';
									errorBlock: [environment openEditor];
									yourself')
]
