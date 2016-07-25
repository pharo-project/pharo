tests
testHasStatusbar
	| browser |
	browser := GLMBrowser new.
	self deny: browser hasStatusbar.
	browser hasStatusbar: true.
	self assert: browser panes size equals: 1.
	self assert: browser panes first name equals: GLMBrowser defaultStatusbarPane.
	self assert: browser statusbarPane == browser panes first