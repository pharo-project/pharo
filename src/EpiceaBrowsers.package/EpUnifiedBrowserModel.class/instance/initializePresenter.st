initialization
initializePresenter

	super initializePresenter.
	
	logModel viewClass: EpLogBrowser defaultViewClass.
	
	logNodesModel logNodesTreeModel 
		whenSelectedItemChanged: [ :items | self refreshWithLogSelected: items ].
		
	self initializeRefreshOnSessionUpdate.
	self initializeWorkaroundToRefreshOnMonitorLogAnnouncement.