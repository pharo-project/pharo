tests
testDynamic
	| browser childBrowser |
	childBrowser := GLMFinder new.
	browser := GLMTabulator new.
	browser column: #one.
	browser transmit to: #one; andShow: [ :a |
		a custom: (GLMDynamicPresentation new display: [childBrowser]) ].
	browser startOn: 42.
	self assert: browser panes first presentations first currentPresentation first == childBrowser.
	self assert: childBrowser pane == browser panes first 