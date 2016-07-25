initialization
initialize

	sessionHolder := nil asValueHolder.
	labelHolder := nil asValueHolder.
	super initialize.
	
	self whenWindowChanged: [ :w | 
		w whenClosedDo: [ self clear ] ].
	
	sessionHolder whenChangedDo: [:newSession :oldSession |
		self removeActionsForSession: oldSession.
		self registerActionsForSession: newSession.
		self updateToolbarFromSession: newSession.
		self updateStackFromSession: newSession ].
	
	labelHolder whenChangedDo: [ 
		self updateTitle ]