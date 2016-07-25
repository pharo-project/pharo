initialize
initializeWidgets
	self initializePackageList.
	self initializeClassList.
	self initializeProtocolList.
	self initializeSelectorList.
	self initializeMethodBodyPane.
	
	showClassSide := self newCheckBox.
	showClassSide label: 'Class side'.
	showClassSide state: false.
	
	self focusOrder
		add: packages;
		add: classes;
		add: protocols;
		add: selectors;
		add: method