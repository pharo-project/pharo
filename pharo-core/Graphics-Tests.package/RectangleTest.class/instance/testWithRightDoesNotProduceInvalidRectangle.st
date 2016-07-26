tests
testWithRightDoesNotProduceInvalidRectangle
	"self debug: #testWithRightDoesNotProduceInvalidRectangle"

	| rect1 rect2 |
	rect1 := 0@10 corner: 50@60.
	
	"it is possible to adjust the right to the right of the current right side"
	rect2 := rect1 withRight: 100.
	self assert: rect2 right equals: 100.

	"it is possible to adjust the right to the left of the current right side but not over the left side!!!!"
	rect2 := rect1 withRight: 40.
	self assert: rect2 right equals: 40.
	
	"it is possible to adjust the right to be to the left of the current right side but not over the left side itself, at the maximum 
	you will a zero width rectangle!!!!"
	rect2 := rect1 withRight: -10.
	self assert: rect2 right equals: 0.
	self assert: rect2 width isZero
	