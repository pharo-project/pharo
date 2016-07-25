tests-align
testFullAlignCenterShrinkWrapInSpaceFill
	|root brick1 brick2 brick3 increment|
	
	increment := 10.
	
	root := self newBrickWithStaticExtent.
	brick1 := self newBrickWithFullFill.
	brick2 := self newBrickWithFullDependsOnChildren.
	brick3 := self newBrickWithSmallStaticExtent.
	
	brick2
		vAlign: #center;
		hAlign: #center.
		
	root addBrickBack: brick1.
	brick1 addBrickBack: brick2.
	brick2 addBrickBack: brick3.
	
	self assertAllSubbricksAreClean: root.
	self assert: brick2 brickBounds equals: ((self staticExtent - self staticSmallExtent / 2) extent: self staticSmallExtent) ceiling.
	
	brick3
		height: brick3 height + increment;
		width: brick3 width + increment.
	self assertAllSubbricksAreClean: root.
	
	self assert: brick2 brickBounds equals: ((self staticExtent - self staticSmallExtent / 2 - (increment / 2) ) extent: (self staticSmallExtent + increment)) ceiling.
	
	
	
	