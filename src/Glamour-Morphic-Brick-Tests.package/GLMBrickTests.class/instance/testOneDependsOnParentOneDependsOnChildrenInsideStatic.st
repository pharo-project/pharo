tests-layouter
testOneDependsOnParentOneDependsOnChildrenInsideStatic
	|root brick1 brick2 brick3 staticWidth staticHeight|
	
	staticWidth := 20.
	staticHeight := 50.
	
	root := self newBrickWithStaticExtent.
	brick1 := self newBrickFullHeightAndWidthDependsOnParent: 100.
	brick2 := self newBrickWithFullDependsOnChildren.
	brick3 := GLMBrick new width: staticWidth; height: staticHeight; yourself.
	
	root addBrickBack: brick1.
	self assert: root extent equals: self staticExtent.
	self assert: brick1 brickBounds equals: (0@0 extent: self staticExtent).
	
	root addBrickBack: brick2.
	self assert: root extent equals: self staticExtent.
	self assert: brick1 brickBounds equals: (0@0 extent: self staticExtent).
	self assert: brick2 brickBounds equals: (self staticWidth @0 extent: 0@0).
	
	brick2 addBrickBack: brick3.
		
	self assertAllSubbricksAreClean: root.
	
	self assert: root extent equals: self staticExtent.
	self assert: brick1 brickBounds equals: (0@0 extent: (self staticWidth - staticWidth) @ self staticHeight).
	self assert: brick2 brickBounds equals: ((self staticWidth - staticWidth)@0 extent: (staticWidth) @ staticHeight).
	self assert: brick3 brickBounds equals: (0@0 extent: (staticWidth) @ staticHeight).
	