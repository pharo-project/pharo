tests - iterating
testNoneSatisfy

	| element |
	self assert: ( self collectionWithoutNilElements  noneSatisfy: [:each | each notNil not ] ).
	element := self collectionWithoutNilElements anyOne.
	self deny: ( self collectionWithoutNilElements  noneSatisfy: [:each | (each = element)not ] ).