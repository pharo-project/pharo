testing
testSameMethodInSet
	"self debug: #testSameMethodInSet"

	| s rgmethod |
	s := Set new.
	rgmethod := (OrderedCollection>>#size) asRingDefinition.
	s add: rgmethod. 
	self assert: (s includes: rgmethod).
	
	s add: rgmethod. 
	self assert: (s size = 1).
	self assert: (s includes: rgmethod).
	
