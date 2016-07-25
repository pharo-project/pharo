tests
testConditionWhenEmbedded
	| expander tabulator |
	expander := GLMExpander new.
	expander display: [ :x :y | x + y ].
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
	self shouldnt: [ tabulator startOn: 1 ] raise: Error