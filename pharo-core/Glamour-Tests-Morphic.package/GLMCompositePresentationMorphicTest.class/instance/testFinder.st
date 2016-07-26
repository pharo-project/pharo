tests
testFinder
	| finder |
	finder := GLMFinder new.
	finder
		show: [ :a | 
			a title: 'something'.
			a stackedArrangement.
			a list.
			a text ].
	self assert: finder transmission transmissionStrategy  presentations size equals: 2.
	finder startOn: (1 to: 42).
	self assert: finder panes first presentations size equals: 2.
	self assert: (finder panes first presentations arrangement isKindOf: GLMStackedVerticallyArrangement).
	window := finder open.
	self find: LazyTabGroupMorph in: window.