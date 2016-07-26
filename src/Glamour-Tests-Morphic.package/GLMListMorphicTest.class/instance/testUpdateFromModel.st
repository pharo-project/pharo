tests
testUpdateFromModel
	| browser treeMorphOne1 treeMorphTwo1 treeMorphOne2 treeMorphTwo2 |
	browser := GLMTabulator new.
	browser
		column: #one;
		column: #two.
	(browser transmit)
		to: #one;
		andShow: [ :a | a list display: [ :x | x - 3 to: x ] ].
	(browser transmit)
		to: #two;
		from: #one;
		andShow: [ :a | a list display: [ :x | x * 10 to: x * 10 + 2 ] ].
	window := browser openOn: 4.
	treeMorphOne1 := self find: MorphTreeMorph in: window.
	self assert: (treeMorphOne1 model roots collect: #item) equals: #(1 2 3 4) asOrderedCollection.
	((browser paneNamed: #one) port: #selection) value: 2.
	treeMorphTwo1 := self findLast: MorphTreeMorph in: window.
	self assert: (treeMorphTwo1 model roots collect: #item) equals: #(20 21 22) asOrderedCollection.
	browser entity: 8.
	treeMorphOne2 := self find: MorphTreeMorph in: window.
	self assert: (treeMorphOne2 model roots collect: #item) equals: #(5 6 7 8) asOrderedCollection.
	self assert: treeMorphOne1 ~= treeMorphOne2.
	self assert: window submorphs last submorphs last submorphs first submorphs isEmpty.
	((browser paneNamed: #one) port: #selection) value: 5.
	treeMorphTwo2 := self findLast: MorphTreeMorph in: window.
	self assert: (treeMorphTwo2 model roots collect: #item) equals: #(50 51 52) asOrderedCollection.
	self assert: treeMorphTwo1 ~= treeMorphTwo2