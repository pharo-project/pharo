initialization
initialize

	self initializeMorphs.
	super initialize.

	container := PanelMorph new.
	container
		changeTableLayout;
		listDirection: #bottomToLeft.