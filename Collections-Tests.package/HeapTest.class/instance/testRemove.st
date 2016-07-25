basic tests
testRemove
	"self run: #testRemove"

	| heap |
	heap := Heap new.
	self should: [ heap removeFirst ] raise: Error.
	heap add: 5.
	heap removeFirst.
	self assert: heap size = 0.
	heap add: 5.
	self should: [ heap removeAt: 2 ] raise: Error