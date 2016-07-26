tests
testSimpleEmptyPane
	| pane |
	pane := GLMPane new.
	pane browser: GLMBrowser new.
	(pane port: #entity) value: 42.
	self assert: GLMLogger instance announcements size equals: 1.
	self assert: (GLMLogger instance announcements first isKindOf: GLMContextChanged)