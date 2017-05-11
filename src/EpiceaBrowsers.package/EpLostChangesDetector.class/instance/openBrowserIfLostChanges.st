accessing
openBrowserIfLostChanges

	| lostChanges lostChangesLog browser |
	lostChanges := self lostChanges collect: #content.
	lostChanges ifEmpty: [ ^ self ].
	lostChangesLog := EpLog newWithStore: (OmMemoryStore withAll: lostChanges).

	browser := EpLogBrowserModel newWithLog: lostChangesLog.
	browser
		refresh;
		title: 'Epicea - Lost Changes Detected';
		openWithSpec.
