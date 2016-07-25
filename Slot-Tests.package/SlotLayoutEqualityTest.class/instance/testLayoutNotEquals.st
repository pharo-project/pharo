tests
testLayoutNotEquals
 
	self layoutClasses do: [:aLayoutClass |
		(self layoutClasses copyWithout: aLayoutClass) do: [:anotherLayoutClass |
			self 
				assertClassBuiltWith: [:builder | builder layoutClass: aLayoutClass ] 
				isNotEqualToClassBuiltWith: [:builder | builder layoutClass: anotherLayoutClass ]]]