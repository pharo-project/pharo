tests
testColumnLabel
	| browser tableMorph |
	browser := GLMTabulator new.
	browser column: #one.
	(browser transmit)
		to: #one;
		andShow: [ :a | 
					(a table)
						display: [ :x | x - 3 to: x ];
						column: 'Column' evaluated: [ :x | x asString ];
						column: [ :x | 'Number ' , x asString ] evaluated: #asString ].
	window := browser openOn: 4.
	tableMorph := self find: MorphTreeMorph in: window.
	self assert: (tableMorph model roots collect: #item) equals: #(1 2 3 4) asOrderedCollection.
	self assert: tableMorph columns first header label equals: 'Column'.
	self assert: tableMorph columns last header label equals: 'Number 4'