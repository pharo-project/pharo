tests
testMoveDefinitionIntoBlockThatIsAReceiverOfACascadedMessage
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