tests comparing
testEqualToWithMapping
	| tree |
	tree := self treeWithEverything.
	self assert: (tree equalTo: tree withMapping: Dictionary new).
	tree := self treeWithReallyEverything.
	self assert: (tree equalTo: tree withMapping: Dictionary new).
	
	self exampleClasses do: [ :class |
		class selectors do: [ :each |
			tree := class parseTreeFor: each.
			self assert: (tree equalTo: tree withMapping: Dictionary new) ] ]