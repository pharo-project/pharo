tests
testRootParent
	| root |
	root := filesystem root.
	self assert: root parent == root