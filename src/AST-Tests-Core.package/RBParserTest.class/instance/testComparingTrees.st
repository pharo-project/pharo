tests comparing
testComparingTrees
	self compare: self treeWithEverything to: self treeWithEverything.
	self compare: self treeWithReallyEverything to: self treeWithReallyEverything.
	
	self exampleClasses do: [ :class |
		class selectors do: [ :selector | 
			self
				compare: (class parseTreeFor: selector) 
				to: (class parseTreeFor: selector) ] ]