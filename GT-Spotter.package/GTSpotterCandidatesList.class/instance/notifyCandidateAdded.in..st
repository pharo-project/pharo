notifying
notifyCandidateAdded: aCandidateLink in: aProcessor

	self announcer announce: (GTSpotterCandidateAdded new candidateLink: aCandidateLink; category: aProcessor; yourself).