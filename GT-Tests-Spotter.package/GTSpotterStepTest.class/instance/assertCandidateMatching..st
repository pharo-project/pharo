convenience
assertCandidateMatching: aBlock
	| candidates |
	self assert: currentProcessor notNil.
	
	candidates := currentProcessor allFilteredCandidates select: [ :each | aBlock value: each ].
	
	self deny: candidates isEmpty.
	self assert: candidates size = 1.
	
	currentCandidate := candidates anyOne.
	self assert: currentCandidate notNil.
	
	^ currentCandidate