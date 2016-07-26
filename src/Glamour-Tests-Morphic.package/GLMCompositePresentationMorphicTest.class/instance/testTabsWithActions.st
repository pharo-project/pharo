tests
testTabsWithActions
	| browser |
	browser := GLMTabulator new.
	browser column: #one.
	(browser transmit)
		to: #one;
		andShow: [ :a | 
					a list act: #inspect entitled: 'Inspect'.
					a text ].
	window := browser openOn: 42.
	self assert: self compositeTabGroup toolbarMorph submorphs notEmpty.
	self compositeTabGroup selectedPageIndex: 2.
	self assert: self compositeTabGroup toolbarMorph submorphs isEmpty.
	self compositeTabGroup selectedPageIndex: 1.
	self assert: self compositeTabGroup toolbarMorph submorphs notEmpty