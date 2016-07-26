testing
testDifferentObjectButSameMethodInSet
	"self debug: #testDifferentObjectButSameMethodInSet"

	| s rgmethod rgmethod2 |
	s := Set new.
	rgmethod := (OrderedCollection>>#size) asRingDefinition.
	s add: rgmethod. 
	rgmethod2 := (OrderedCollection>>#size) asRingDefinition.
	s add: rgmethod2.
	
	self assert: (s size = 1).
	self assert: (s includes: rgmethod).
	self assert: (s includes: rgmethod2).

