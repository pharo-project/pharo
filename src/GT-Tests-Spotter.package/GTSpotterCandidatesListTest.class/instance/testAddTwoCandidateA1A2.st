as yet unclassified
testAddTwoCandidateA1A2
	|link11 link12 link21 link22 category1 category2|
	
	category1 := candidates addProcessor: GTSpotterA1.
	category2 := candidates addProcessor: GTSpotterA2.
	
	link11 := candidates addCandidate: 11 in: GTSpotterA1.
	link12 := candidates addCandidate: 12 in: GTSpotterA1.
	link21 := candidates addCandidate: 21 in: GTSpotterA2.
	link22 := candidates addCandidate: 22 in: GTSpotterA2.
	
	self assert: (link11 nextLink = link12).
	self assert: (link11 previousLink = link22).
	
	self assert: (link12 nextLink = link21).
	self assert: (link12 previousLink = link11).
	
	self assert: (link21 nextLink = link22).
	self assert: (link21 previousLink = link12).
	
	self assert: (link22 nextLink = link11).
	self assert: (link22 previousLink = link21).
	
	self assert: (candidates head = link11).
	self assert: (candidates tail = link22).
	
	self assert: ((category1 headLink) = link11).
	self assert: ((category1 tailLink) = link12).
	
	self assert: ((category2 headLink) = link21).
	self assert: ((category2 tailLink) = link22).
	
	