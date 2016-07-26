tests
testAddPane
	| browser pane |
	browser := GLMBrowser new.
	self assert: browser panes isEmpty.
	pane := GLMPane new.
	browser addPane: pane.
	self assert: pane browser equals: browser