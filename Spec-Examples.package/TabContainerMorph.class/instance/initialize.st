initialization
initialize

	super initialize.
	
	self changeProportionalLayout.
	container := PanelMorph new
		changeTableLayout;
		listDirection: #bottomToTop;
		color: Color white;
		yourself.
	
	tab := PluggableTabBarMorph on: self.
	tab width: 150.
	tab color: Color white darker darker.
	
	self addAll.