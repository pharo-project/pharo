initialization
initialize

	container := PanelMorph new.
	
	container
		changeTableLayout;
		listDirection: #bottomToLeft.
	
	self instantiateMorphs.
	super initialize.