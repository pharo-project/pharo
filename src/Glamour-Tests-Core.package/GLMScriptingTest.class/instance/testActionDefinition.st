tests
testActionDefinition
	| browser |
	browser := GLMTabulator new.
	browser column: #one.
	browser
		transmit to: #one;
		andShow: [ :a | 
					a text act: [  ] entitled: ''.
					a list act: [  ] entitled: ''.
					a tree act: [  ] entitled: '' ].
	browser startOn: #(42).
	browser panes first presentations do: [ :each | self assert: each actions size equals: 1 ]