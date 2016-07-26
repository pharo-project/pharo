as yet unclassified
testRemoveCandidatesA1FromA1A2A3
	|link21 link22 link23 link31 link32 link33 category1 category2 category3|
	
	category1 := candidates addProcessor: GTSpotterA1.
	category2 := candidates addProcessor: GTSpotterA2.
	category3 := candidates addProcessor: GTSpotterA3.
	
	candidates addCandidate: 11 in: GTSpotterA1.
	candidates addCandidate: 12 in: GTSpotterA1.
	candidates addCandidate: 13 in: GTSpotterA1.
	link21 := candidates addCandidate: 21 in: GTSpotterA2.
	link22 := candidates addCandidate: 22 in: GTSpotterA2.
	link23 := candidates addCandidate: 23 in: GTSpotterA2.
	link31 := candidates addCandidate: 31 in: GTSpotterA3.
	link32 := candidates addCandidate: 32 in: GTSpotterA3.
	link33 := candidates addCandidate: 33 in: GTSpotterA3.
	
	candidates removeAllCandidatesOf: GTSpotterA1.
	
	self assert: (link21 nextLink = link22).
	self assert: (link21 previousLink = link33).
	
	self assert: (link22 nextLink = link23).
	self assert: (link22 previousLink = link21).
	
	self assert: (link23 nextLink = link31).
	self assert: (link23 previousLink = link22).
	
	self assert: (link31 nextLink = link32).
	self assert: (link31 previousLink = link23).
	
	self assert: (link32 nextLink = link33).
	self assert: (link32 previousLink = link31).
	
	self assert: (link33 nextLink = link21).
	self assert: (link33 previousLink = link32).
		
	self assert: (candidates head = link21).
	self assert: (candidates tail = link33).
	
	self assert: ((category1 headLink) = nil).
	self assert: ((category1 tailLink) = nil).
	
	self assert: ((category2 headLink) = link21).
	self assert: ((category2 tailLink) = link23).
	
	self assert: ((category3 headLink) = link31).
	self assert: ((category3 tailLink) = link33).