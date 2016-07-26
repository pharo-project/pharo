tests
testLayoutEquals

	self layoutClasses do: [:aLayoutClass |
		self 
			assertClassBuiltWith: [:builder | builder layoutClass: aLayoutClass ] 
			isEqualToClassBuiltWith: [:builder | builder layoutClass: aLayoutClass ] ]