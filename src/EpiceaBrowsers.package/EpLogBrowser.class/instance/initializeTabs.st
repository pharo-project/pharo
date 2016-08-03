initialization
initializeTabs
	entryContentModel showOptions: false.
	entryContentTabModel
		label: 'Selection';
		icon: (Smalltalk ui icons iconNamed: #smallOpenIcon);
		model: self entryContentModel;
		closeable: false.
	filtersTabModel
		label: 'Active filters';
		icon: (Smalltalk ui icons iconNamed: #smallFindIcon);
		model: self filtersModel;
		closeable: false.
	tabManagerModel
		addTab: entryContentTabModel;
		addTab: filtersTabModel