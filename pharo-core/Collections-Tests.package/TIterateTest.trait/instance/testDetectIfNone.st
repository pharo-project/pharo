tests - iterating
testDetectIfNone

	| res element |
	res := self collectionWithoutNilElements  detect: [:each | each notNil not] ifNone: [100].
	self assert: res  = 100.
	
	element := self collectionWithoutNilElements anyOne.
	res := self collectionWithoutNilElements  detect: [:each | each = element] ifNone: [100].
	self assert: res  = element.

	
	