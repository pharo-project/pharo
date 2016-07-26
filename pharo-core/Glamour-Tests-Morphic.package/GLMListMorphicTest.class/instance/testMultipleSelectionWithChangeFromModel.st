tests
testMultipleSelectionWithChangeFromModel
	| browser firstTreeMorph secondTreeMorph |
	browser := GLMTabulator new.
	browser column: #one.
	(browser transmit)
		to: #one;
		andShow: [ :a | 
					(a list)
						beMultiple;
						display: [ :x | x - 2 to: x ] ].
	window := browser openOn: 4.
	firstTreeMorph := window submorphs last submorphs first submorphs first submorphs first.
	self assert: (firstTreeMorph model roots collect: #item) equals: #(2 3 4) asOrderedCollection.
	((browser paneNamed: #one) port: #selection) value: #(2 3).
	self assert: ((browser paneNamed: #one) port: #selection) value asOrderedCollection equals: #(2 3) asOrderedCollection.
	(browser pane port: #entity) value: 42.
	secondTreeMorph := window submorphs last submorphs first submorphs first submorphs first.
	self assert: (secondTreeMorph model roots collect: #item) equals: #(40 41 42) asOrderedCollection.
	((browser paneNamed: #one) port: #selection) value: #(40 41).
	self
		assert: ((browser paneNamed: #one) port: #selection) value asOrderedCollection
		equals: #(40 41) asOrderedCollection.
	self assert: firstTreeMorph ~= secondTreeMorph