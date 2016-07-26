asserting
assertAllSubbricksAreDirty: aBrick

	aBrick withAllSubbricks do: [ :each | self assertBrickDirty: each ]