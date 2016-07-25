private
privateAddCandidate: aCandidateLink weightedInNotFull: aProcessorLink
	|head tail|
	
	head := aProcessorLink headLink.
	tail := aProcessorLink tailLink.

	"fast check if we need to add it before first"
	(aCandidateLink weight > head weight)
		ifTrue: [ head := self candidates add: aCandidateLink beforeLink: head ]
		ifFalse: [
	"fast check if we need to add it after the last"
	(aCandidateLink weight > tail weight)
		ifFalse: [ tail := self candidates add: aCandidateLink afterLink: tail ]
		ifTrue: [  ] ]