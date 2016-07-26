tests
testSettingLeftDoesNotProduceInvalidRectangle
  
	| rect1 rect2 |
	
	rect1 := 0@0 corner: 40@40.

	"the left side of new rectangle wants to be beyond the right side of it"
	rect2 := rect1 left: 50.
	
	self assert: rect2 left equals: 40.
	self assert: rect2 right equals: 50.