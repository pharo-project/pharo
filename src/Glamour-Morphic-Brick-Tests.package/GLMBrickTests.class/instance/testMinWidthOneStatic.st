tests-geometry
testMinWidthOneStatic
	|brick|
	
	brick := self newBrickWithSmallStaticExtent.
	brick minWidth: self staticWidth.
	brick doLayoutForce.
	self assertAllSubbricksAreClean: brick.
	
	self assert: brick brickBounds equals: (0@0 extent: self staticWidth @ self staticSmallHeight)