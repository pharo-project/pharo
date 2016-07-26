asserting
assertAllSubbricksAreClean: aBrick

	aBrick withAllSubbricks do: [ :each | self assertBrickClean: each ]