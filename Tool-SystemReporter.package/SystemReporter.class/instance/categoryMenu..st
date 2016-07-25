accessing-categories
categoryMenu: aMenu
	^ aMenu
		addTitle: 'Categories';
		add: 'Select all' selector: #selectAllCategories;
		add: 'Select none' selector: #selectNoCategories;
		addLine;
		add: 'Refresh' selector: #refresh;
		yourself