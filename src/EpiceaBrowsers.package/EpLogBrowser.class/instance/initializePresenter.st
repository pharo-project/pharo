initialization
initializePresenter
	super initializePresenter.
	
	self initializeFiltersModel.
	self initializeItemsModel.
	self initializeTabs.
	
	self view initializeBrowser.