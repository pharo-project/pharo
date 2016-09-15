initialization
initializePresenter

	super initializePresenter.
	
	logModel viewClass: EpLogBrowser defaultViewClass.
	
	logNodesModel logNodesTreeModel 
		whenSelectedItemChanged: [ :item | self refreshWithLogSelected: item ] .
		
	self initializeWorkaroundToRefreshOnMonitorLogAnnouncement.