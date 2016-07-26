initialization
initialize

	super initialize.
	
	self 
	initializeAppearance;
	initializeLayout;
	beResetable;
	resetAlpha.
	
	fadingFactor := self defaultFadingFactor.