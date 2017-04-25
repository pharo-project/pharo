tests
testOneStackedVerticallyWithTitle
	| browser |
	browser := GLMTabulator new.
	browser column: #one.
	(browser transmit)
		to: #one;
		andShow: [ :a | 
					a stackedVerticallyArrangement.
					a title: 'title'.
					a list ].
	window := browser openOn: 42.
	self assert: self compositeTabGroup submorphs notEmpty.
	self assert: self compositeTabGroup submorphs size equals: 2.
	self assert: self compositeTabGroup contentMorph class equals: GLMPanelMorph.
	self assert: self compositeTabGroup contentMorph submorphs size equals: 1.
	self assert: self compositeTabGroup contentMorph submorphs first class equals: GLMPanelMorph.
	self assert: self compositeTabGroup contentMorph submorphs first submorphs size equals: 1.
	self assert: self compositeTabGroup contentMorph submorphs first submorphs first class equals: LazyTabGroupMorph