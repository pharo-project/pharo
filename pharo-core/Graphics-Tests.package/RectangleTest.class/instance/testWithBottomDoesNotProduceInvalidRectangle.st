tests
testWithBottomDoesNotProduceInvalidRectangle
	"self debug: #testWithBottomDoesNotProduceInvalidRectangle"

	| rect1 rect2 |
	rect1 := 0@10 corner: 50@60.
	
	"it is possible to adjust the bottom below the current bottom side"
	rect2 := rect1 withBottom: 100.
	self assert: rect2 bottom equals: 100.

	"it is possible to adjust the bottom above the current bottom side"
	rect2 := rect1 withBottom: 40.
	self assert: rect2 bottom equals: 40.
	
	"it is possible to adjust the bottom above the current bottom side but not above the top"
	rect2 := rect1 withBottom: -10.
	self assert: rect2 bottom equals: 10.
	self assert: rect2 height isZero
	