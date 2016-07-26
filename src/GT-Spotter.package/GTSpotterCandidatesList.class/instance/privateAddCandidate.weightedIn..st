private
privateAddCandidate: aCandidateLink weightedIn: aProcessorLink

	^ aProcessorLink isFull
		ifTrue: [ self privateAddCandidate: aCandidateLink weightedInFull: aProcessorLink ]
		ifFalse: [ self privateAddCandidate: aCandidateLink weightedInNotFull: aProcessorLink ]