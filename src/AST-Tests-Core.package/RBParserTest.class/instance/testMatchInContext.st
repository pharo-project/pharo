tests matching
testMatchInContext
	| tree |
	tree := self treeWithEverything.
	self assert: (tree match: tree inContext: Dictionary new).
	tree := self treeWithReallyEverything.
	self assert: (tree match: tree inContext: Dictionary new).
	
	self exampleClasses do: [ :class |
		class selectors do: [ :each |
			tree := class parseTreeFor: each.
			self assert: (tree match: tree inContext: Dictionary new) ] ]