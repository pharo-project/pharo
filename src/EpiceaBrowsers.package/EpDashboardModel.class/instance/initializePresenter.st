initialization
initializePresenter 
	super initializePresenter.
	
	self initializeCreateLogButtonModel.
	self initializeEnableMonitorButtonModel.
	self initializeOpenSettingsButtonModel.

	"Listen announcers"
	self initializeRefreshEnableMonitorButtonModelOnMonitorUpdate.