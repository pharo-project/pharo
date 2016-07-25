tests
testAccordionExpanded
	| browser |
	browser := GLMTabulator new.
	browser column: #one.
	browser transmit to: #one; andShow: [ :a | 
		a accordionArrangement.
		a list title: 'List'.
		a text title: 'Text'].
	window := browser openOn: 42.
	self assert: self compositePanel submorphs first expanded.
	self assert: self compositePanel submorphs last expanded not.