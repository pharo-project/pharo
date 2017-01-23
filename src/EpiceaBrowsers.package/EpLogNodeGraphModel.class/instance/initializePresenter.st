initialization
initializePresenter 
	super initializePresenter.
	
	self initializeSelectedDirectoryButtonModel.
	self initializeLogNodesTreeModel.
	self initializeRefreshButtonModel.
	self initializeCreateLogButtonModel.
	self initializeEnableMonitorButtonModel.

	"Listen announcers"
	self initializeRefreshEnableMonitorButtonModelOnMonitorUpdate.