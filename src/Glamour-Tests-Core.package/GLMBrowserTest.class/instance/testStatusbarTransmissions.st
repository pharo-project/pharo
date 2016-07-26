tests
testStatusbarTransmissions
	| browser pane |
	browser := GLMBrowser new.
	browser addStatusbar.
	pane := GLMPane new.
	browser addPane: pane.
	self assert: browser transmissions size equals: 1.
	self assert: browser transmissions first origins size equals: 1.
	self assert: browser transmissions first origins first equals: (pane port: #status).
	self assert: browser transmissions first destination equals: (browser panes first port: #entity).
	self assert: (browser transmissions first presentations first isKindOf: GLMLabelPresentation)