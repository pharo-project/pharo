tests
testRenameTemporary
	| refactoring |
	refactoring := RBRenameTemporaryRefactoring 
		renameTemporaryFrom: (self 
				convertInterval: (15 to: 19)
				for: (RBLintRuleTest sourceCodeAt: #openEditor))
		to: 'asdf'
		in: RBLintRuleTest
		selector: #openEditor.
	self executeRefactoring: refactoring.
	self assert: ((refactoring model classNamed: #RBLintRuleTest) parseTreeFor: #openEditor) = (RBParser parseMethod: 'openEditor
								| asdf |
								asdf := self failedRules.
								asdf isEmpty ifTrue: [^self].
								asdf size == 1 ifTrue: [^asdf first viewResults]')