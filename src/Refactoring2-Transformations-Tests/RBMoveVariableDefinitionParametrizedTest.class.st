Class {
	#name : #RBMoveVariableDefinitionParametrizedTest,
	#superclass : #RBAbstractRefactoringTest,
	#category : #'Refactoring2-Transformations-Tests-SingleParametrized'
}

{ #category : #'building suites' }
RBMoveVariableDefinitionParametrizedTest class >> testParameters [
	^ ParametrizedTestMatrix new
		addCase: { #rbClass -> RBMoveVariableDefinitionToSmallestValidScopeTransformation };
		yourself
]

{ #category : #accessing }
RBMoveVariableDefinitionParametrizedTest >> constructor [
	^ #bindTight:in:selector:
]

{ #category : #'failure tests' }
RBMoveVariableDefinitionParametrizedTest >> testFailureInvalidSelector [

	self shouldFail: (self createRefactoringWithArguments: {
				 (21 to: 24).
				 RBRefactoryTestDataApp.
				 #caller3 })
]

{ #category : #'failure tests' }
RBMoveVariableDefinitionParametrizedTest >> testFailureNoMoveDefinition [

	self shouldFail: (self createRefactoringWithArguments: {
				 (21 to: 24).
				 RBRefactoryTestDataApp.
				 #noMoveDefinition })
]

{ #category : #'failure tests' }
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

{ #category : #tests }
RBMoveVariableDefinitionParametrizedTest >> testMoveDefinition [
	| refactoring |
	refactoring := self createRefactoringWithArguments:
		{ (19 to: 22) . RBRefactoryTestDataApp . #moveDefinition }.
	self executeRefactoring: refactoring.

	self assert: ((refactoring model classNamed: #RBRefactoryTestDataApp) parseTreeForSelector: #moveDefinition) equals: (self parseMethod: 'moveDefinition
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

{ #category : #tests }
RBMoveVariableDefinitionParametrizedTest >> testMoveDefinitionIntoBlockThatIsAReceiverOfACascadedMessage [
	| refactoring |
	refactoring := self createRefactoringWithArguments:
		{ (47 to: 57) . RBRefactoryTestDataApp . #referencesConditionFor: }.
	self executeRefactoring: refactoring.
	self assert: ((refactoring model classNamed: #RBRefactoryTestDataApp) parseTreeForSelector: #referencesConditionFor:) equals: (self parseMethod: 'referencesConditionFor: aClass
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
