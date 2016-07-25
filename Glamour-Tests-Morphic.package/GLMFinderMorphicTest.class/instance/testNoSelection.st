tests
testNoSelection
	| browser treeMorph treeNodeMorph |
	browser := GLMFinder new.
	browser show: [:a | a tree noSelection].
	window := browser openOn: ($a to: $d).
	treeMorph := window submorphs last submorphs first submorphs first submorphs first submorphs first.
	treeNodeMorph := treeMorph scroller submorphs first.
	self send: treeMorph mouseUpOnItem: treeNodeMorph.
	self assert: browser panes size equals: 1