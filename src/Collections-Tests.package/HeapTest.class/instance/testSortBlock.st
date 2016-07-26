basic tests
testSortBlock
	"self run: #testSortBlock"

	| heap |
	heap := Heap withAll: #(1 3 5).
	self assert: heap asArray = #(1 3 5).
	
	heap sortBlock: [ :e1 :e2 | e1 >= e2 ].
	self assert: heap asArray = #(5 3 1)
