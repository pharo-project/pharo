tests
testARectangleDoesNotContainItsCorner
	self
		deny: (rectangle1 containsPoint: rectangle1 corner)