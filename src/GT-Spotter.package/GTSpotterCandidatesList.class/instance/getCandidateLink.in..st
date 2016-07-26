candidates
getCandidateLink: aCandiate in: aProcessor
	"returns a corresponding candidateLink, that belongs to a processor"
	(self getCandidateLinks: (self getProcessorLink: aProcessor))
		do: [ :each | each value = aCandiate ifTrue: [ ^ each ] ].
	
	^ nil
	