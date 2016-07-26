as yet unclassified
testAddThreeCandidateA1
	|link1 link2 link3 category|
	
	category := candidates addProcessor: GTSpotterA1.
	link1 := candidates addCandidate: 11 in: GTSpotterA1.
	link2 := candidates addCandidate: 12 in: GTSpotterA1.
	link3 := candidates addCandidate: 13 in: GTSpotterA1.
	
	self assert: (link1 nextLink = link2).
	self assert: (link1 previousLink = link3).
	
	self assert: (link2 nextLink = link3).
	self assert: (link2 previousLink = link1).
	
	self assert: (link3 nextLink = link1).
	self assert: (link3 previousLink = link2).
	
	self assert: (candidates head = link1).
	self assert: (candidates tail = link3).
	self assert: ((category headLink) = link1).
	self assert: ((category tailLink) = link3).
	
	