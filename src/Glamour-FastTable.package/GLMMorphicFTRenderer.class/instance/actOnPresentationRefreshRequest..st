callbacks
actOnPresentationRefreshRequest: anAnnouncement

	tableMorph window ifNil: [ ^ self ].
	tableMorph window isActive ifFalse: [ ^ self ].
	(tableMorph window boundsInWorld containsRect: tableMorph boundsInWorld) ifFalse: [ ^ self ].
  " tableModel resetElements."
	tableModel resetDataCache.
	tableMorph refresh