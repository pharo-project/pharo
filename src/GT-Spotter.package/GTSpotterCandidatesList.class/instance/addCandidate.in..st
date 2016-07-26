adding/removing
addCandidate: anObjectOrLink in: aProcessor
	"before adding candidates we need to define all categories, because their order is important"
	|processorLink candidateLink|
	
	"processorLink is DoubleLink"
	processorLink := self getProcessorLink: aProcessor.
	
	candidateLink := anObjectOrLink asSpotterCandidateLink processorLink: processorLink.
	
	"if head is nil, it means there are no candidates yet"
	candidateLink := (processorLink isEmpty)
		ifTrue: [ self privateAddCandidate: candidateLink inEmpty: processorLink ]
		ifFalse: [ self privateAddCandidate: candidateLink inNotEmpty: processorLink ].
		
	self updateCycleConnectionFor: candidateLink.
	
	self notifyCandidateAdded: candidateLink in: aProcessor.
	
	^ candidateLink
	