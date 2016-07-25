as yet unclassified
testCategoryLinkByCandidateLinkThreeA1A2A3
	|link11 link12 link13 link21 link22 link23 link31 link32 link33 category1 category2 category3|
	
	category1 := candidates addProcessor: GTSpotterA1.
	category2 := candidates addProcessor: GTSpotterA2.
	category3 := candidates addProcessor: GTSpotterA3.
	
	link11 := candidates addCandidate: 11 in: GTSpotterA1.
	link12 := candidates addCandidate: 12 in: GTSpotterA1.
	link13 := candidates addCandidate: 13 in: GTSpotterA1.
	link21 := candidates addCandidate: 21 in: GTSpotterA2.
	link22 := candidates addCandidate: 22 in: GTSpotterA2.
	link23 := candidates addCandidate: 23 in: GTSpotterA2.
	link31 := candidates addCandidate: 31 in: GTSpotterA3.
	link32 := candidates addCandidate: 32 in: GTSpotterA3.
	link33 := candidates addCandidate: 33 in: GTSpotterA3.
	
	self assert: (category1 = (candidates getProcessorLinkByCandidateLink: link11)).
	self assert: (category1 = (candidates getProcessorLinkByCandidateLink: link12)).
	self assert: (category1 = (candidates getProcessorLinkByCandidateLink: link13)).
	
	self assert: (category2 = (candidates getProcessorLinkByCandidateLink: link21)).
	self assert: (category2 = (candidates getProcessorLinkByCandidateLink: link22)).
	self assert: (category2 = (candidates getProcessorLinkByCandidateLink: link23)).
	
	self assert: (category3 = (candidates getProcessorLinkByCandidateLink: link31)).
	self assert: (category3 = (candidates getProcessorLinkByCandidateLink: link32)).
	self assert: (category3 = (candidates getProcessorLinkByCandidateLink: link33)).