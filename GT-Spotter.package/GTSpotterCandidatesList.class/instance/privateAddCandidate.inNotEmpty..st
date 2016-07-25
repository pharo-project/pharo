private
privateAddCandidate: aCandidateLink inNotEmpty: aProcessorLink
	
	"aCandidateLink weight
		ifNotNil: [ ^ self privateAddCandidate: aCandidateLink weightedIn: aProcessorLink ]."
	
	self candidates add: aCandidateLink afterLink: (aProcessorLink tailLink).
	aProcessorLink incrementSize.
	"updating category tail"
	aProcessorLink tailLink: aCandidateLink.
	^ aCandidateLink