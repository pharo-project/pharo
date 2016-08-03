startup - shutdown
startUp: isImageStarting

	| log freshLog browser |
	isImageStarting ifFalse: [^self].
	EpMonitor current isEnabled ifFalse: [^self]. "Imposible to recover anything since we were not recording events"

	log := EpMonitor current log.
	freshLog := log copyFromHead. 
	freshLog refresh. "Update head to most recently recorded in file"
	freshLog headReference = log headReference ifTrue: [^self]. "No lost events"

	"Then, there are lost events"
	browser := EpLogBrowser newWithLog: freshLog.

	log headReference isNull ifFalse: [ 		"Hide known events... i.e. show only lost events"
		| lastKnownEventTime |
		lastKnownEventTime := log timeAt: log head.
		browser addFilter: (EpPluggableFilter after: lastKnownEventTime) ].

	browser
		refresh;
		open.