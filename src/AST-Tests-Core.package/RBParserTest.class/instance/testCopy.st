tests
testCopy
	| tree |
	tree := self treeWithEverything.
	self compare: tree to: tree copy.
	tree := self treeWithReallyEverything.
	self compare: tree to: tree copy.
	
	self exampleClasses do: [ :class |
		class selectors do: [ :each |
			tree := class parseTreeFor: each.
			self compare: tree to: tree copy ] ]