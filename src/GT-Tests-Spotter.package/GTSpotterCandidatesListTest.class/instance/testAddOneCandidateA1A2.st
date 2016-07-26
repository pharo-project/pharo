as yet unclassified
testAddOneCandidateA1A2
	|link11 link21 category1 category2|
	
	category1 := candidates addProcessor: GTSpotterA1.
	category2 := candidates addProcessor: GTSpotterA2.
	link11 := candidates addCandidate: 11 in: GTSpotterA1.
	link21 := candidates addCandidate: 21 in: GTSpotterA2.
	
	self assert: (link11 nextLink = link21).
	self assert: (link11 previousLink = link21).
	
	self assert: (link21 nextLink = link11).
	self assert: (link21 previousLink = link11).
	
	self assert: (candidates head = link11).
	self assert: (candidates tail = link21).
	
	self assert: ((category1 headLink) = link11).
	self assert: ((category1 tailLink) = link11).
	
	self assert: ((category2 headLink) = link21).
	self assert: ((category2 tailLink) = link21).
	
	