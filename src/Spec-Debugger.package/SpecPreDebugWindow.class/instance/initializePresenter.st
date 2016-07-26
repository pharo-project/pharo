initialization
initializePresenter

	super initializePresenter.
	
	debugger whenChangedDo: [ :aDebugger |
		aDebugger 
			ifNil: [ self clearWidget ] 
			ifNotNil:  [ self rebuildWidget ]	 ]
