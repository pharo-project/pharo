tests
testDefaultPane
	| presentation pane |
	presentation := GLMPresentation new.
	pane := presentation pane.
	self assert: pane presentations first pane == pane.
	self assert: pane presentations pane == pane.
	self assert: pane presentations first == presentation