tests
testOneClass

	| pointNode sorted |
	pointNode := SortHierarchicallyIndentNode.
	sorted := SortHierarchically sortClasses: {pointNode}.
	self assert: sorted size = 1.
	self assert: (sorted includes: pointNode)