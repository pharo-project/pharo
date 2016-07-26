tests
testIsRendered
	self assert: tabs pages first isRendered not.
	tabs selectedPageIndex: 1.
	self assert: tabs pages first isRendered.
	self assert: tabs pages second isRendered not