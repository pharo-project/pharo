failure tests
testOverriden
	self shouldWarn: (RBInlineMethodRefactoring 
			inline: (self 
					convertInterval: (15 to: 26)
					for: (RBLintRuleTest sourceCodeAt: #failedRules))
			inMethod: #failedRules
			forClass: RBLintRuleTest)