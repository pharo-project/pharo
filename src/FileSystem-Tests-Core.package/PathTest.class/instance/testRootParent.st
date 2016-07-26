tests
testRootParent
	| root |
	root := Path root.
	self assert: root parent == root