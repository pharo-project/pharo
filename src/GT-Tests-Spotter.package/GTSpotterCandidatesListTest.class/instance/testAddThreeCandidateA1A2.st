as yet unclassified
testAddThreeCandidateA1A2
	|link11 link12 link13 link21 link22 link23 category1 category2|
	
	category1 := candidates addProcessor: GTSpotterA1.
	category2 := candidates addProcessor: GTSpotterA2.
	
	link11 := candidates addCandidate: 11 in: GTSpotterA1.
	link12 := candidates addCandidate: 12 in: GTSpotterA1.
	link13 := candidates addCandidate: 13 in: GTSpotterA1.
	link21 := candidates addCandidate: 21 in: GTSpotterA2.
	link22 := candidates addCandidate: 22 in: GTSpotterA2.
	link23 := candidates addCandidate: 23 in: GTSpotterA2.
	
	self assert: (link11 nextLink = link12).
	self assert: (link11 previousLink = link23).
	
	self assert: (link12 nextLink = link13).
	self assert: (link12 previousLink = link11).
	
	self assert: (link13 nextLink = link21).
	self assert: (link13 previousLink = link12).
	
	self assert: (link21 nextLink = link22).
	self assert: (link21 previousLink = link13).
	
	self assert: (link22 nextLink = link23).
	self assert: (link22 previousLink = link21).
	
	self assert: (link23 nextLink = link11).
	self assert: (link23 previousLink = link22).
	
	self assert: (candidates head = link11).
	self assert: (candidates tail = link23).
	
	self assert: ((category1 headLink) = link11).
	self assert: ((category1 tailLink) = link13).
	
	self assert: ((category2 headLink) = link21).
	self assert: ((category2 tailLink) = link23).