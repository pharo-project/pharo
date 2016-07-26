tests-layouter
test100FillBricksInsideEachOtherIndideStatic
	|root current|
	
	root := self newBrickWithStaticExtent.
	current := root.
	1 to: 100 do: [ :index |
		current := current addBrickBack: self newBrickWithFullFill ].
	
	self assertAllSubbricksAreClean: root.
	
	self assert: current extent equals: root extent
	