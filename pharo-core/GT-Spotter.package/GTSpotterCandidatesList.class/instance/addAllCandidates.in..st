adding/removing
addAllCandidates: aCollection in: aProcessor
	| allLinks |
	allLinks := OrderedCollection new.
	aCollection
		do: [ :each | 
			| processorLink candidateLink |
			processorLink := self getProcessorLink: aProcessor.
			candidateLink := each asSpotterCandidateLink processorLink: processorLink.	"if head is nil, it means there are no candidates yet"
			candidateLink := processorLink isEmpty
				ifTrue: [ self privateAddCandidate: candidateLink inEmpty: processorLink ]
				ifFalse: [ self privateAddCandidate: candidateLink inNotEmpty: processorLink ].
			self updateCycleConnectionFor: candidateLink.
			allLinks add: candidateLink ].
	self announcer announce:
			(GTSpotterAllCandidatesAdded new
				candidateLinks: allLinks;
				category: aProcessor;
				yourself)