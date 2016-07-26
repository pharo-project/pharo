tests
testTreeWithDisplay
	| browser treeCollection |
	treeCollection := { #root1->{#child1->{} . #child2->{}} . #root2 ->{} }.
	browser := GLMTabulator new.
	browser column: #one.
	browser transmit to: #one; andShow: [ :a |
		a tree
			display: [:each | each value ];
			children: [:x | x value ] ].
	window := browser openOn: #something -> treeCollection.
	self assert: browser panes first presentations first displayValue == treeCollection