as yet unclassified
testPreviousA1Category

	|link11 link12 link13 category1 |
	
	category1 := candidates addProcessor: GTSpotterA1.
	
	link11 := candidates addCandidate: 11 in: GTSpotterA1.
	link12 := candidates addCandidate: 12 in: GTSpotterA1.
	link13 := candidates addCandidate: 13 in: GTSpotterA1.
	
	self assert: (category1 = (candidates previousProcessorLinkOf: category1)).
	
	