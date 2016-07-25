tests-geometry
testMaxWidthOneStatic
	|brick|
	
	brick := self newBrickWithStaticExtent.
	brick maxWidth: self staticSmallWidth.
	brick doLayoutForce.
	self assertAllSubbricksAreClean: brick.
	
	self assert: brick brickBounds equals: (0@0 extent: self staticSmallWidth @ self staticHeight)