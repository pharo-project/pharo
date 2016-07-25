tests
testOuterEntity
	| browser |
	browser := GLMTabulator new.
	browser
		column: #one;
		column: #two.
	browser
		transmit to: #one;
		andShow: [ :a | a text ].
	browser
		transmit to: #two;
		fromOutsidePort: #entity;
		andShow: [ :a | a text ].
	browser startOn: 42.
	self assert: browser transmissions first origins anyOne name equals: #entity.
	self assert: browser transmissions last origins anyOne name equals: #entity.
	self assert: browser transmissions size equals: 2