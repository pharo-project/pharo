Class {
	#name : #RBMoveVariableDefinitionTest,
	#superclass : #RBRefactoringTest,
	#category : #Refactoring-Tests-Core
}

{ #category : #tests }
RBMoveVariableDefinitionTest >> testMoveDefinition [
	| refactoring |
	refactoring := RBMoveVariableDefinitionRefactoring 
		bindTight: (self 
				convertInterval: (19 to: 22)
				for: (RBRefactoryTestDataApp sourceCodeAt: #moveDefinition))
		in: RBRefactoryTestDataApp
		selector: #moveDefinition.
	self executeRefactoring: refactoring.
	self assert: ((refactoring model classNamed: #RBRefactoryTestDataApp) parseTreeFor: #moveDefinition) = (RBParser parseMethod: 'moveDefinition
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
		bindTight: (self 
				convertInterval: (48 to: 58)
				for: (RBRefactoryTestDataApp sourceCodeAt: #referencesConditionFor:))
		in: RBRefactoryTestDataApp
		selector: #referencesConditionFor:.
	self executeRefactoring: refactoring.
	self assert: ((refactoring model classNamed: #RBRefactoryTestDataApp) parseTreeFor: #referencesConditionFor:) = (RBParser parseMethod: 'referencesConditionFor: aClass 
								| environment  |
								^(RBCondition withBlock: 
										[| association |association := Smalltalk associationAt: aClass name
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
	self shouldFail: (RBMoveVariableDefinitionRefactoring 
			bindTight: (self 
					convertInterval: (21 to: 24)
					for: (RBRefactoryTestDataApp sourceCodeAt: #moveDefinition))
			in: RBRefactoryTestDataApp
			selector: #noMoveDefinition)
]

{ #category : #tests }
RBMoveVariableDefinitionTest >> testNonExistantName [
	self
		shouldFail: (RBMoveVariableDefinitionRefactoring 
				bindTight: (1 to: 10)
				in: RBLintRuleTest
				selector: #name1);
		shouldFail: (RBMoveVariableDefinitionRefactoring 
				bindTight: (self 
						convertInterval: (44 to: 54)
						for: (RBLintRuleTest sourceCodeAt: #displayName))
				in: RBLintRuleTest
				selector: #displayName);
		shouldFail: (RBMoveVariableDefinitionRefactoring 
				bindTight: (self 
						convertInterval: (16 to: 25)
						for: (RBLintRuleTest sourceCodeAt: #displayName))
				in: RBLintRuleTest
				selector: #displayName)
]
