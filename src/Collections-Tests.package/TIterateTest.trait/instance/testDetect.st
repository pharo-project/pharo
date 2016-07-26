tests - iterating
testDetect

	| res element |
	element := self collectionWithoutNilElements anyOne .
	 
	res := self collectionWithoutNilElements  detect: [:each | each = element].
	self assert: (res  = element).

	
	