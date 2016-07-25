failure tests
testBadName
	self
		shouldFail: (RBRenameTemporaryRefactoring 
				renameTemporaryFrom: (self 
						convertInterval: (15 to: 19)
						for: (RBLintRuleTest sourceCodeAt: #openEditor))
				to: 'name'
				in: RBLintRuleTest
				selector: #openEditor);
		shouldFail: (RBRenameTemporaryRefactoring 
				renameTemporaryFrom: (self 
						convertInterval: (15 to: 19)
						for: (RBLintRuleTest sourceCodeAt: #openEditor))
				to: 'rules'
				in: RBLintRuleTest
				selector: #openEditor);
		shouldFail: (RBRenameTemporaryRefactoring 
				renameTemporaryFrom: (self 
						convertInterval: (15 to: 19)
						for: (RBLintRuleTest sourceCodeAt: #openEditor))
				to: 'DependentFields'
				in: RBLintRuleTest
				selector: #openEditor);
		shouldFail: (RBRenameTemporaryRefactoring 
				renameTemporaryFrom: (self 
						convertInterval: (15 to: 19)
						for: (RBLintRuleTest sourceCodeAt: #openEditor))
				to: 'a b'
				in: RBLintRuleTest
				selector: #openEditor)