tests
testOneTabWithActions
	| browser |
	browser := GLMTabulator new.
	browser column: #one.
	(browser transmit)
		to: #one;
		andShow: [ :a | a list act: #inspect entitled: 'Inspect' ].
	window := browser openOn: 42.
	self assert: self compositeMorph class equals: PanelMorph.
	self assert: self compositeMorph submorphs size equals: 2