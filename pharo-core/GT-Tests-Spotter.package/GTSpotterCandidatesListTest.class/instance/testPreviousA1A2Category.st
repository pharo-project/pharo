as yet unclassified
testPreviousA1A2Category

	|link11 link12 link13 link21 link22 link23 category1 category2|
	
	category1 := candidates addProcessor: GTSpotterA1.
	category2 := candidates addProcessor: GTSpotterA2.
	
	link11 := candidates addCandidate: 11 in: GTSpotterA1.
	link12 := candidates addCandidate: 12 in: GTSpotterA1.
	link13 := candidates addCandidate: 13 in: GTSpotterA1.
	link21 := candidates addCandidate: 21 in: GTSpotterA2.
	link22 := candidates addCandidate: 22 in: GTSpotterA2.
	link23 := candidates addCandidate: 23 in: GTSpotterA2.
	
	self assert: (category1 = (candidates previousProcessorLinkOf: category2)).
	self assert: (category2 = (candidates previousProcessorLinkOf: category1)).
	
	