tests
testStatusbarTransmissionsTriggered
	| browser pane |
	browser := GLMTabulator new.
	browser addStatusbar.
	self assert: browser statusbarPane notNil.
	self assert: browser statusbarPane ~= browser.
	pane := (GLMPane in: browser) name: #one.
	browser addPane: pane.
	self assert: browser transmissions size equals: 1.
	(pane port: #status) value: #test.
	self assert: browser statusbarPane ~= browser.
	self assert: (browser statusbarPane port: #entity) value equals: #test.
	self assert: browser statusbarPane presentations size equals: 1.
	self assert: (browser statusbarPane presentations first isKindOf: GLMLabelPresentation)