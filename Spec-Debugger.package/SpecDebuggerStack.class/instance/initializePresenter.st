initialization
initializePresenter

	super initializePresenter.

	sessionHolder whenChangedDo: [ :aSession |
		self updateContentFromSession: aSession ] 
