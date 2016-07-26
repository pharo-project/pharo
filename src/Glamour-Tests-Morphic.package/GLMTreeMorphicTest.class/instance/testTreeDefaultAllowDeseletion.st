tests
testTreeDefaultAllowDeseletion
	| browser treeNodeMorph |
	browser := GLMFinder new.
	browser show: [:a | a tree].
	window := browser openOn: ($a to: $d).
	treeNodeMorph := self treeVisibleItems first.
	self send: self tree mouseUpOnItem: treeNodeMorph.
	self send: self tree mouseUpOnItem: treeNodeMorph.
	self assert: (browser panes first port: #selection) value equals: $a