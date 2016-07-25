tests
testListDoubleClick
	| browser treeMorph treeNodeMorph |
	browser := GLMFinder new.
	browser show: [ :a | a list ].
	window := browser openOn: ($a to: $d).
	treeMorph := window submorphs last submorphs first submorphs first submorphs first submorphs first.
	treeNodeMorph := treeMorph scroller submorphs first.
	self send: treeMorph mouseUpWithShift: false onItem: treeNodeMorph.
	self send: treeMorph doubleClickWithShift: false onItem: treeNodeMorph.
	self assert: (browser panes first port: #strongSelection) value equals: $a