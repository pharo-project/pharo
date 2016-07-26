initialization
initialize
	super initialize.
	self initializeSelections.
	self hideColumnHeaders.
	self dataSource: FTPluggableIconListMorphAdaptorDataSource new.
