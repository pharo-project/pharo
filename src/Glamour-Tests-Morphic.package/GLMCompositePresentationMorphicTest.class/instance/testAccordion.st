tests
testAccordion
	| browser |
	browser := GLMTabulator new.
	browser column: #one.
	(browser transmit)
		to: #one;
		andShow: [ :a | 
					a accordionArrangement.
					a list title: 'List'.
					a text title: 'Text' ].
	window := browser openOn: 42.
	self assert: self compositePanel submorphs notEmpty.
	self assert: self compositePanel submorphs size equals: 2.
	self assert: self compositePanel submorphs first class equals: ExpanderMorph.
	self assert: self compositePanel submorphs last class equals: ExpanderMorph