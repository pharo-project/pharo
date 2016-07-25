tests
testSetup
	| browser pane1 pane2 transmission |
	browser := GLMExplicitBrowser new.
	pane1 := browser addNewPane.
	pane2 := browser addNewPane.
	transmission := (GLMTransmission new)
		addActiveOrigin: (pane1 port: #selection);
		destination: (pane2 port: #entity);
		yourself.
	browser addTransmission: transmission.
	self assert: browser panes size equals: 2.
	self assert: browser transmissions size equals: 1.
	self assert: browser panes first == pane1.
	self assert: browser panes last == pane2.
	self assert: pane1 browser == browser.
	self assert: pane2 browser == browser.
	self assert: browser transmissions first == transmission