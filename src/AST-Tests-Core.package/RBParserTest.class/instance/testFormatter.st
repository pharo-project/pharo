tests
testFormatter
	self exampleClasses do: [ :class |
		class selectors do: [ :selector | 
		self 
			compare: (class parseTreeFor: selector)
			to: (self parserClass parseMethod: (class parseTreeFor: selector) printString) ] ]