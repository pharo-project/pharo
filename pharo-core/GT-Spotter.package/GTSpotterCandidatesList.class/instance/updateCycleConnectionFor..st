candidates
updateCycleConnectionFor: aCandidateLink

	"if link is the last setting nextLink to head"
	(self candidates tail = aCandidateLink) ifTrue: [
		aCandidateLink nextLink: self candidates head.
		self candidates head previousLink: aCandidateLink ].
	
	"if link is the first setting previousLink to tail"
	(self candidates head = aCandidateLink) ifTrue: [
		aCandidateLink previousLink: self candidates tail.
		self candidates tail nextLink: aCandidateLink ].