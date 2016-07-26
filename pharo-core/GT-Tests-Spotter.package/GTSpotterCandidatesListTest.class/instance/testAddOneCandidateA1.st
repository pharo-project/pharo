as yet unclassified
testAddOneCandidateA1
	|link category|
	
	category := candidates addProcessor: GTSpotterA1.
	link := candidates addCandidate: 11 in: GTSpotterA1.
	
	self assert: (link nextLink = link).
	self assert: (link previousLink = link).
	self assert: (candidates head = link).
	self assert: (candidates tail = link).
	self assert: ((category headLink) = link).
	self assert: ((category tailLink) = link).
	
	