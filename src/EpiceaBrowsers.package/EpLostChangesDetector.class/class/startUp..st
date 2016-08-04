startup - shutdown
startUp: isImageStarting

	| log freshLog browser |
	(isImageStarting and: [ 
		self isEnabled and: [ "Detector can be disabled as a setting"
		EpMonitor current isEnabled	and: [ "Nothing to recover if not recording"
		Smalltalk isHeadless not ]]])  "Can't browse if UI disabled"
			ifFalse: [^self].

	log := EpMonitor current log.
	freshLog := log copyFromHead. 
	freshLog refresh. "Update head to most recently recorded in file"
	freshLog headReference = log headReference ifTrue: [^self]. "No lost events"

	"Then, there are lost events"
	browser := EpLogBrowser newWithLog: freshLog.

	"Hide known events... i.e. show only lost events"
	log headReference isNull ifFalse: [
		log
			entryFor: log headReference
			ifPresent: [ :head | 
				| lastKnownEventTime |
				lastKnownEventTime := log timeAt: head.
				browser addFilter: (EpPluggableFilter after: lastKnownEventTime) ]
			ifAbsent: [^self]. "Lost log is not present"
		].

	browser
		refresh;
		open.