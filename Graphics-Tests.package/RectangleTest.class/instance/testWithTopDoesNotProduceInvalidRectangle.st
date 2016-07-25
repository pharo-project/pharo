tests
testWithTopDoesNotProduceInvalidRectangle
	"self debug: #testWithTopDoesNotProduceInvalidRectangle"

	| rect1 rect2 |
	rect1 := 0@10 corner: 50@60.
	
	"it is possible to adjust the top above of the current top side"
	rect2 := rect1 withTop: -10.
	self assert: rect2 top equals: -10.

	"it is possible to adjust the top below of the current top side"
	rect2 := rect1 withTop: 40.
	self assert: rect2 top equals: 40.
	
	"it is possible to adjust the top below of the current top side but not below the bottom"
	rect2 := rect1 withTop: 70.
	self assert: rect2 top equals: 60.
	self assert: rect2 height isZero
	