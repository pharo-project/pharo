announcement handling
jobStarted: aJobStart

	| newEntry aJob |
	aJob := aJobStart job.

	"We only care in these cases"
	aJob owner isEpiceaInterestingJobOwner ifFalse: [ ^ self ].

	newEntry := self addEvent: aJob owner asEpiceaEvent.

	entryReferenceByEvent
		at: aJob
		put: (log referenceTo: newEntry)