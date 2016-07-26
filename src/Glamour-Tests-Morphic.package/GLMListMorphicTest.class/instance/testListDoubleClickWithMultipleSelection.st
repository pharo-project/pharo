tests
testListDoubleClickWithMultipleSelection
	| browser treeMorph treeNodeMorph lastTreeNodeMorph |
	browser := GLMFinder new.
	browser show: [:a | a tree beMultiple].
	window := browser openOn: ($a to: $d).
	treeMorph := window submorphs last submorphs first submorphs first submorphs first submorphs first.
	treeNodeMorph := treeMorph scroller submorphs first.
	lastTreeNodeMorph := treeMorph scroller submorphs last.
	self send: treeMorph mouseUpWithShift: false onItem: treeNodeMorph.
	self send: treeMorph mouseUpWithShift: true onItem: lastTreeNodeMorph.
	self send: treeMorph doubleClickWithShift: false onItem: treeNodeMorph.
	self assert: (browser panes first port: #strongSelection) value asSet equals: ($a to: $d) asSet