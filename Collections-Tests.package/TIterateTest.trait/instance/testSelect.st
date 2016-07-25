tests - iterating
testSelect

	| result element |
	result := self collectionWithoutNilElements select: [ :each | each notNil].
	self assert: result size equals: self collectionWithoutNilElements size.
	
	element := self collectionWithoutNilElements anyOne.
	result := self collectionWithoutNilElements select: [ :each | (each = element) not].
	self assert: result size equals: (self collectionWithoutNilElements size - 1).
	