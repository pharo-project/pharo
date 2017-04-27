accessing
cachedLogEntries
	"Answer log entries.
	Check size since the log might be current Monitor's, so it can grow."

	^ (cachedLogEntries isNotNil and: [ cachedLogEntries size = log entriesCount ])
		ifTrue: [ cachedLogEntries ]
		ifFalse: [
			'Reading log'
				displayProgressFrom: 0 to: 1 
				during: [ cachedLogEntries := log entries reversed ]
			]
