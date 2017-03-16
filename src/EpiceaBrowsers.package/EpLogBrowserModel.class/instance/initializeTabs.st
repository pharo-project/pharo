initialization
initializeTabs

	entryContentModel showOptions: false.
	entryContentTabModel
		label: 'Selection';
		icon: (self iconNamed: #smallOpenIcon);
		model: self entryContentModel;
		closeable: false.
	filtersTabModel
		label: self activeFiltersLabel;
		icon: (self iconNamed: #smallFindIcon);
		model: self filtersModel;
		closeable: false.
	tabManagerModel
		addTab: entryContentTabModel;
		addTab: filtersTabModel