tests
testWithLeftDoesNotProduceInvalidRectangle
	"self debug: #testWithLeftDoesNotProduceInvalidRectangle"

	| rect1 rect2 |
	rect1 := 0@10 corner: 50@60.
	
	"it is possible to adjust the left to the left of the current left side"
	rect2 := rect1 withLeft: -10.
	self assert: rect2 left equals: -10.

	"it is possible to adjust the left to the right of the current left side but not over the righ side!!!!"
	rect2 := rect1 withLeft: 40.
	self assert: rect2 left equals: 40.
	
	"it is possible to adjust the left to be to the right of the current left side but not over the righ side, at the maximum 
	you will a zero width rectangle!!!!"
	rect2 := rect1 withLeft: 80.
	self assert: rect2 left equals: 50.
	self assert: rect2 width isZero
	