tests
testWithDynamicPresentation
	| browser |
	browser := GLMTabulator new.
	browser column: #one.
	browser transmit
		to: #one;
		andShow: [ :a | a list ].
	browser transmit
		from: #one;
		toWatcher;
		andShow: [ :a | a dynamic display: [ :x | GLMRubricTextPresentation new ] ].
	window := browser openOn: (1 to: 42).
	((browser paneNamed: #one) port: #selection) value: 42.
	(Delay forMilliseconds: 1000) wait.
	self assert: (self find: RubScrolledTextMorph in: GLMWatcherWindow uniqueInstance) text equals: '42'