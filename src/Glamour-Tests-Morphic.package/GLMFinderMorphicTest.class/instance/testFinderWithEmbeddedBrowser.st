tests
testFinderWithEmbeddedBrowser

	| navigator finder tree1 tree2 |
	navigator := GLMTabulator new.
	navigator row: #content; row: #details.
	navigator transmit to: #content; andShow: [:a | a list display: #allSuperclasses].
	navigator transmit from: #content; to: #details; andShow: [ :a | a text ].
	navigator transmit from: #content; toOutsidePort: #selection.
	finder := GLMFinder new.
	finder show: [:a | a custom: navigator].

	window := finder openOn: String.
	(finder panes first port: #selection) value: Collection.
	
	self assert: finder panes first presentations first ~= finder panes last presentations first.

	tree1 := self find: MorphTreeMorph in: window submorphs last submorphs last submorphs first.
	tree2 := self findLast: MorphTreeMorph in: window submorphs last submorphs last submorphs last.
	self assert: tree1 notNil.
	self assert: tree2 notNil.
	self assert: tree1 ~= tree2.
	self assert: tree1 model glamourPresentation pane ~= tree2 model glamourPresentation pane.
	self assert: tree1 model glamourPresentation ~= tree2 model glamourPresentation.