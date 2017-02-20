tests
testStackedVertically
	| browser |
	browser := GLMTabulator new.
	browser column: #one.
	(browser transmit)
		to: #one;
		andShow: [ :a | 
					a stackedVerticallyArrangement.
					a list title: 'list'.
					a text title: 'text' ].
	window := browser openOn: 42.
	self assert: self compositeMorph class equals: GLMPanelMorph.
	self assert: self compositeMorph submorphs size equals: 2.
	self assert: self compositeMorph submorphs size equals: 2.
	self assert: self compositeMorph submorphs first class equals: LazyTabGroupMorph