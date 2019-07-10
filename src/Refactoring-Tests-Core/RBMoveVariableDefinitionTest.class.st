Class {
	#name : #RBMoveVariableDefinitionTest,
	#superclass : #RBRefactoringTest,
	#category : #'Refactoring-Tests-Core'
}

{ #category : #tests }
RBMoveVariableDefinitionTest >> testMoveDefinition [
	| refactoring |
	refactoring := RBMoveVariableDefinitionRefactoring
		bindTight: (19 to: 22)
		in: RBRefactoryTestDataApp
		selector: #moveDefinition.
	self executeRefactoring: refactoring.

	self assert: ((refactoring model classNamed: #RBRefactoryTestDataApp) parseTreeFor: #moveDefinition) equals: (self parseMethod: 'moveDefinition
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
RBMoveVariableDefinitionTest >> testMoveDefinitionIntoBlockThatIsAReceiverOfACascadedMessage [
	| refactoring |
	refactoring := RBMoveVariableDefinitionRefactoring
		bindTight: (48 to: 58)
		in: RBRefactoryTestDataApp
		selector: #referencesConditionFor:.
	self executeRefactoring: refactoring.
	self assert: ((refactoring model classNamed: #RBRefactoryTestDataApp) parseTreeFor: #referencesConditionFor:) equals: (self parseMethod: 'referencesConditionFor: aClass 
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

{ #category : #tests }
RBMoveVariableDefinitionTest >> testNoMoveDefinition [
	self
		shouldFail:
			(RBMoveVariableDefinitionRefactoring
				bindTight: (21 to: 24)
				in: RBRefactoryTestDataApp
				selector: #noMoveDefinition)
]

{ #category : #tests }
RBMoveVariableDefinitionTest >> testNonExistantName [
	self
		shouldFail:
			(RBMoveVariableDefinitionRefactoring
				bindTight: (1 to: 10)
				in: RBLintRuleTestData
				selector: #name1);
		shouldFail:
			(RBMoveVariableDefinitionRefactoring
				bindTight: (44 to: 54)
				in: RBLintRuleTestData
				selector: #displayName);
		shouldFail:
			(RBMoveVariableDefinitionRefactoring
				bindTight: (16 to: 25)
				in: RBLintRuleTestData
				selector: #displayName)
]
