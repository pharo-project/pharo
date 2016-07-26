tests
testFormatter
	self exampleClasses do: [ :class |
		class selectors do: [ :selector | 
		self 
			compare: (class parseTreeFor: selector)
			to: (RBParser parseMethod: (class parseTreeFor: selector) printString) ] ]