initialization
initializePresenter

	super initializePresenter.

	self initializeItemsModel.
	self initializeToolbarModel.
	self initializeEntryContentModel.
	
	self beHistoryMode.