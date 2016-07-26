as yet unclassified
testNextA1A2EmptyCategory

	|link11 link12 link13 category1 category2|
	
	category1 := candidates addProcessor: GTSpotterA1.
	category2 := candidates addProcessor: GTSpotterA2.
	
	link11 := candidates addCandidate: 11 in: GTSpotterA1.
	link12 := candidates addCandidate: 12 in: GTSpotterA1.
	link13 := candidates addCandidate: 13 in: GTSpotterA1.
	
	self assert: (category1 = (candidates nextProcessorLinkOf: category1)).
	self assert: (category1 = (candidates nextProcessorLinkOf: category2)).
	
	