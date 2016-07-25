initialization
initialize

	container := PanelMorph new.
	self instantiateMorphs.
	
	container
		changeTableLayout;
		listDirection: #bottomToLeft.
	
	super initialize.