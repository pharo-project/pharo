tests
testCopyInContext
	| tree |
	tree := self treeWithEverything.
	self compare: tree to: (tree copyInContext: Dictionary new).
	tree := self treeWithReallyEverything.
	self compare: tree to: (tree copyInContext: Dictionary new).
	
	self exampleClasses do: [ :class |
		class selectors do: [ :each |
			tree := class parseTreeFor: each.
			self compare: tree to: (tree copyInContext: Dictionary new) ] ]