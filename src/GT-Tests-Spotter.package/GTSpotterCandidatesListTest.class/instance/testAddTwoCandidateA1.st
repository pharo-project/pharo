as yet unclassified
testAddTwoCandidateA1
	|link1 link2 category|
	
	category := candidates addProcessor: GTSpotterA1.
	link1 := candidates addCandidate: 11 in: GTSpotterA1.
	link2 := candidates addCandidate: 12 in: GTSpotterA1.
	
	self assert: (link1 nextLink = link2).
	self assert: (link1 previousLink = link2).
	
	self assert: (link2 nextLink = link1).
	self assert: (link2 previousLink = link1).
	
	self assert: (candidates head = link1).
	self assert: (candidates tail = link2).
	self assert: ((category headLink) = link1).
	self assert: ((category tailLink) = link2).
	
	