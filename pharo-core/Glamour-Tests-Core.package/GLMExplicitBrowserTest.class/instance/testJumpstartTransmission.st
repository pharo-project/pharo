tests
testJumpstartTransmission
	| browser presentation |
	browser := GLMExplicitBrowser new.
	presentation := GLMPresentation new.
	browser addNewPane.
	browser
		addTransmission:
			((GLMTransmission new)
				ensureReplacePresentationsStrategy;
				addActiveOrigin: (browser pane port: #entity);
				destination: (browser panes first port: #entity);
				addPresentation: presentation;
				yourself).
	self assert: (browser panes first port: #entity) value isNil.
	self assert: browser panes first presentations isEmpty.
	(browser pane port: #entity) value: #foo.
	self assert: (browser panes first port: #entity) value equals: #foo.
	self assert: browser panes first presentations size equals: 1.
	self assert: browser panes first presentations first rootPrototype equals: presentation