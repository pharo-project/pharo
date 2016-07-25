tests
testCopyNotTheSameRegistry

	| pane newPane |
	pane := GLMPane new.
	newPane := pane copy.
	self assert: pane glmSubscriptions ~~ newPane glmSubscriptions