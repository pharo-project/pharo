as yet unclassified
testRemoveCandidatesA1A2A3FromA1A2A3
	|category1 category2 category3|
	
	category1 := candidates addProcessor: GTSpotterA1.
	category2 := candidates addProcessor: GTSpotterA2.
	category3 := candidates addProcessor: GTSpotterA3.
	
	candidates addCandidate: 11 in: GTSpotterA1.
	candidates addCandidate: 12 in: GTSpotterA1.
	candidates addCandidate: 13 in: GTSpotterA1.
	candidates addCandidate: 21 in: GTSpotterA2.
	candidates addCandidate: 22 in: GTSpotterA2.
	candidates addCandidate: 23 in: GTSpotterA2.
	candidates addCandidate: 31 in: GTSpotterA3.
	candidates addCandidate: 32 in: GTSpotterA3.
	candidates addCandidate: 33 in: GTSpotterA3.
	
	candidates removeAllCandidatesOf: GTSpotterA1.
	candidates removeAllCandidatesOf: GTSpotterA3.
	candidates removeAllCandidatesOf: GTSpotterA2.
			
	self assert: (candidates head = nil).
	self assert: (candidates tail = nil).
	
	self assert: ((category1 headLink) = nil).
	self assert: ((category1 tailLink) = nil).
	
	self assert: ((category2 headLink) = nil).
	self assert: ((category2 tailLink) = nil).
	
	self assert: ((category3 headLink) = nil).
	self assert: ((category3 tailLink) = nil).