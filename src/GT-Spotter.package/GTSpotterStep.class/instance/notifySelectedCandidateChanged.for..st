notifying
notifySelectedCandidateChanged: aCandidateLink for: anEventType
	| aCandidate |
	
	self
		assert: [ aCandidateLink isNotNil ]
		description: [ 'aCandidateLink must not be nil' ].

	aCandidate := aCandidateLink candidate.
	self announcer announce: (GTSpotterSelectedCandidateChanged new select: aCandidate).
	self spotter notifyCandidateSelected: aCandidate for: anEventType