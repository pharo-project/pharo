testing - geometry
testBoundingBoxOfSubmorphs

	| subMorph |
	subMorph := morph copy
		topLeft: morph topLeft + 5;
		yourself.

	morph addMorph: subMorph.
	
	self assert: morph boundingBoxOfSubmorphs equals: subMorph bounds.
		