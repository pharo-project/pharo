initialization
initializePresenter

	super initializePresenter.
	
	logNodesModel logNodesTreeModel
		whenSelectedItemChanged: [ :items | self refreshWithLogSelected: items ].
		
	self initializeRefreshOnSessionUpdate.
	self initializeWorkaroundToRefreshOnMonitorLogAnnouncement.