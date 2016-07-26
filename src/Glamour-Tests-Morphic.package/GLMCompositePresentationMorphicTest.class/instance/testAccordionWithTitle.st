tests
testAccordionWithTitle
	| browser |
	browser := GLMTabulator new.
	browser column: #one.
	(browser transmit)
		to: #one;
		andShow: [ :a | 
					a accordionArrangement.
					a title: 'title'.
					a list.
					a text ].
	window := browser openOn: 42.
	self assert: self compositeTabGroup submorphs notEmpty.
	self assert: self compositeTabGroup submorphs size equals: 2.
	self assert: self compositeTabGroup contentMorph class equals: PanelMorph.
	self assert: self compositeTabGroup contentMorph submorphs first submorphs size equals: 2.
	self assert: self compositeTabGroup contentMorph submorphs first submorphs first class equals: ExpanderMorph