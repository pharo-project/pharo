tests - iterating
testAnySastify

	| element |
	" when all elements satisty the condition, should return true :"
	self assert: ( self collectionWithoutNilElements anySatisfy: [:each | each notNil ]).
	
	" when only one element satisfy the condition, should return true :"
	element := self collectionWithoutNilElements anyOne.
	self assert: ( self collectionWithoutNilElements  anySatisfy: [:each | (each = element)  ]   ).
	
	" when all elements don't satisty the condition, should return false :"
	self deny: ( self collectionWithoutNilElements anySatisfy: [:each | (each notNil) not ]).
