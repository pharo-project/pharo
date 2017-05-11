selecting
select: aCandidateLink dueTo: anEventType
	"Select a given candidate due to provided event type.
	A selection reason is used for statistical purposes and helps us
	better understand how users use spotter and as result improve it.
	aCandidateLink must not be nil.
	anEventType must not be nil"

	self
		assert: [ aCandidateLink isNotNil ]
		description: [ 'Candidate link must not be nil' ].

	self
		assert: [ aCandidateLink processor isNotNil ]
		description: [ 'Candidate must have assigned category (processor)' ].
	
	"A given candidate is already selected, do nothing"
	(self isSelected: aCandidateLink)
		ifTrue: [ ^ self ].

	self spotter
		after: [
			selected := aCandidateLink.
			self notifySelectedCandidateChanged: aCandidateLink for: anEventType ]
		do: [ self spotter notifyCategorySelected: self selected processor ]
		ifTrue: [ (self isCategorySelected: aCandidateLink processor) not ]