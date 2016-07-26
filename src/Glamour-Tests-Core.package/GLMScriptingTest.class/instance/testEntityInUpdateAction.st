tests
testEntityInUpdateAction
	| browser inputFromActionBlock |
	browser := GLMTabulator new.
	browser column: #one.
	browser
		transmit to: #one;
		andShow: [ :a | a text populate: #foo on: $m with: [ :list :input | inputFromActionBlock := input ] ].
	browser startOn: 42.
	browser panes first presentations first actions first actOn: browser panes first presentations first.
	self assert: inputFromActionBlock equals: 42