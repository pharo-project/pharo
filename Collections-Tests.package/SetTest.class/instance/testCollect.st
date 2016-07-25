tests - iterating
testCollect

	| res element collection|
	collection := self collectionWithoutNilElements.
	element := self element.
	res := collection collect: [:each | element].
	
	self assert:(res class == self classToBeTested).
	self assert: res size = 1.
	
	res do: [ :each|
		self assert: each = element ]