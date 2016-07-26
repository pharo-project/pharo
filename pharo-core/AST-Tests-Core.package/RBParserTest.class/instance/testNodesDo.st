tests
testNodesDo
	| size |
	size := 0.
	self treeWithEverything nodesDo: [:e | size := size + 1].
	self assert: size = 19