tests - iterating
testAllSatisfy

	| element |
	" when all element  satisfy the condition, should return true : "
	self assert: ( self collectionWithoutNilElements  allSatisfy: [:each | (each notNil) ] ).
	
	" when all element don't satisfy the condition, should return false : "
	self deny: ( self collectionWithoutNilElements  allSatisfy: [:each | (each notNil) not ] ).
	
	" when only one element doesn't satisfy the condition' should return false'"
	element := self collectionWithoutNilElements anyOne.
	self deny: ( self collectionWithoutNilElements  allSatisfy: [:each | (each = element) not] ).