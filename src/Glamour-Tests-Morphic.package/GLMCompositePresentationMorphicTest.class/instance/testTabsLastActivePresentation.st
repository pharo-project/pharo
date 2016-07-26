tests
testTabsLastActivePresentation
	| browser |
	browser := GLMTabulator new.
	browser column: #one.
	(browser transmit)
		to: #one;
		andShow: [ :a | 
					a list.
					a text ].
	window := browser openOn: 42.
	self compositeTabGroup selectedPageIndex: 2.
	self assert: browser panes first lastActivePresentation notNil.
	self assert: (browser panes first lastActivePresentation isKindOf: GLMRubricTextPresentation)