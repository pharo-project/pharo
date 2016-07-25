tests
testTreeMultipleSelectionWithCancellation
	| browser treeNodeMorph lastTreeNodeMorph |
	browser := GLMFinder new.
	browser show: [:a | a tree beMultiple allowDeselection].
	window := browser openOn: ($a to: $d).
	treeNodeMorph := self treeVisibleItems first.
	lastTreeNodeMorph := self treeVisibleItems last.
	self send: self tree mouseUpOnItem: treeNodeMorph.
	self send: self tree mouseUpWithShift: true onItem: lastTreeNodeMorph.
	self send: self tree mouseUpOnItem: treeNodeMorph.
	self send: self tree mouseUpOnItem: treeNodeMorph.
	self assert: (browser panes first port: #selection) value isNil