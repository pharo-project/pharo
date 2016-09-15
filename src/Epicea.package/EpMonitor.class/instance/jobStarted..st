announcement handling
jobStarted: aJobStart

	| aJob |
	aJob := aJobStart job.

	"We only care in these cases"
	aJob owner isEpiceaInterestingJobOwner ifFalse: [ ^ self ].

	self 
		addEvent: aJob owner asEpiceaEvent 
		newEntryDo: [ :newEntry |
			entryReferenceByEvent
				at: aJob
				put: (log referenceTo: newEntry) ]