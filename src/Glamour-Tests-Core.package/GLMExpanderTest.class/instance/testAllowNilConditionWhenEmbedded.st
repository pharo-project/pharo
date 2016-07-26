tests
testAllowNilConditionWhenEmbedded
	| expander tabulator |
	expander := GLMExpander new.
	expander allowNil.
	expander display: [ :x :y | x + y ].	"should lead to an error because nil is allowed in the block"
	expander show: [ :a | a text display: [ :x :y | x asString , y asString ] ].
	tabulator := GLMTabulator new.
	tabulator
		column: #one;
		column: #two.
	(tabulator transmit)
		to: #two;
		fromOutsidePort: #entity;
		from: #one port: #nilPort;
		andShow: [ :a | a custom: expander ].
	self should: [ tabulator startOn: 1 ] raise: Error