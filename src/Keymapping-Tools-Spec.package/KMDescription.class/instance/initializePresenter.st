initialization
initializePresenter
		
	categories whenChangedDo: [:col | self initCategoryList:col ].
	okToolbar okAction: [ self delete ].