accessing-packages
packageMenu: aMenu
	^ aMenu
		addTitle: 'Packages';
		add: 'Select all' selector: #selectAllPackages;
		add: 'Select inversion' selector: #selectInversePackages;
		add: 'Select none' selector: #selectNoPackages;
		addLine;
		add: 'Filter...' selector: #filterPackages;
		addLine;
		add: 'Refresh' selector: #updatePackages;
		yourself.