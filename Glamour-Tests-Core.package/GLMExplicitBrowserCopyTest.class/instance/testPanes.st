tests
testPanes
	self assert: copiedBrowser panes size equals: 2.
	self assert: copiedBrowser panes first ~~ browser panes first.
	self assert: copiedBrowser panes last ~~ browser panes last.
	self assert: copiedBrowser panes first browser == copiedBrowser.
	self assert: copiedBrowser panes last browser == copiedBrowser.
	self assert: copiedBrowser panes first ports size equals: 1.
	self assert: copiedBrowser panes first ports first name equals: #selection.
	self assert: copiedBrowser panes first ports first pane == copiedBrowser panes first.
	self assert: copiedBrowser panes last ports size equals: 2.
	self assert: copiedBrowser panes last ports first name equals: #selection.
	self assert: copiedBrowser panes last ports last name equals: #entity