factory
buildWidget
	^ TabManager new
		model: self model;
		setTabs: self getTabs;
		when: #tabSelected send: #tabSelected: to: self;
		vResizing: #spaceFill;
		hResizing: #spaceFill;
		yourself