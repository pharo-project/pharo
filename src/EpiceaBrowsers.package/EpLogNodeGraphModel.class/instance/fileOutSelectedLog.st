initialization
fileOutSelectedLog
	
	| item |
	item := logNodesTreeModel selectedItem.
	item ifNil: [ ^self ].
	EpOmbuExporter askFileNameAndFileOut: item content log entries in: self directory.
	self refresh.