tests
testTrasmissions
	self assert: copiedBrowser transmissions size equals: 2.
	self assert: copiedBrowser transmissions first ~~ browser transmissions first.
	self assert: copiedBrowser transmissions last ~~ browser transmissions last.
	self assert: (copiedBrowser transmissions first destination pane browser isKindOf: GLMNoBrowser).
	self assert: copiedBrowser transmissions last destination pane browser == copiedBrowser.
	self assert: copiedBrowser transmissions first origins first pane browser == copiedBrowser.
	self assert: copiedBrowser transmissions last origins first pane browser == copiedBrowser