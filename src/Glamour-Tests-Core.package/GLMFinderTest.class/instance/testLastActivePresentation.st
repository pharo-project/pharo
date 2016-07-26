tests
testLastActivePresentation
	| browser |
	browser := GLMFinder new.
	browser
		show: [ :a | 
			a list title: 'a'.
			a text title: 'b'.
			a table title: 'c' ].
	browser startOn: 1.
	self assert: browser lastActivePresentation isNil.
	(browser panes first port: #selection) value: 2.
	self assert: browser lastActivePresentation isNil.
	browser panes last lastActivePresentation: browser panes last presentations last.
	self assert: browser lastActivePresentation equals: browser panes last presentations last.
	(browser panes first port: #selection) value: 3.
	self assert: browser panes last lastActivePresentation title equals: 'c'