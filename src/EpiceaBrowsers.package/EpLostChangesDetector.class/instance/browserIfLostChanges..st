accessing
browserIfLostChanges: aBlock

	| freshLog freshStore browser lastKnowHeadReferenceOrNil |
	lastKnowHeadReferenceOrNil := self monitorLog headReference.
	freshStore := self monitorLog store lowLevelFileStoreIfNone: [ ^ self ]. "No file to recover from"
	freshStore refresh.
	freshLog := EpLog newWithStore: freshStore.
	freshLog headReference = lastKnowHeadReferenceOrNil ifTrue: [ ^ self ]. "No lost events"

	"Then, there are lost events"
	browser := EpLogBrowserModel newWithLog: freshLog.

	"Hide known events... i.e. show only lost events"
	lastKnowHeadReferenceOrNil isNull ifFalse: [
		freshLog
			entryFor: lastKnowHeadReferenceOrNil
			ifPresent: [ :head | 
				| lastKnownEventTime |
				lastKnownEventTime := freshLog timeAt: head.
				browser addFilter: (EpPluggableFilter after: lastKnownEventTime) ]
			ifAbsent: [ ^ self ]. "Lost log is not present"
		].

	browser
		refresh;
		title: 'Epicea - Lost Changes Detected'.
		
	browser filteredEntries ifNotEmpty: [ aBlock value: browser ].