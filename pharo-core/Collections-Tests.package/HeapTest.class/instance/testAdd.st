basic tests
testAdd
	"self run: #testAdd" 

	| heap |
	heap := Heap new.
	self assert: heap size = 0.
	heap add: 3.
	self assert: heap size = 1.
	self assert: heap isEmpty not.
	self assert: heap first = 3.
	heap add: 2.
	self assert: heap size = 2.
	self assert: heap first = 2.
	