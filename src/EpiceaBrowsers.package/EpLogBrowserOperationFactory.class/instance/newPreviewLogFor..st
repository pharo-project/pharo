private
newPreviewLogFor: rawEvents

	| aLog filter latestEvents |
	self flag: #fix.
	aLog := EpLog newWithStore: (OmMemoryStore withAll: rawEvents).
	logBrowserModel := EpLogBrowserModel newWithLog: aLog.
	filter := EpLatestCodeChangeFilter new logBrowserModel: logBrowserModel; yourself.
	latestEvents := aLog entries 
		select: [:each | filter accepts: each ]
		thenCollect: [:each | each content ].

	^ EpLog newWithStore: (OmMemoryStore withAll: latestEvents)
