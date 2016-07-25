accessing-classes
classMenu: aMenu
	^ aMenu
		addTitle: 'Classes';
		add: 'Browse' selector: #browseClass;
		addLine;
		add: 'Select all' selector: #selectAllClasses;
		add: 'Select unit tests' selector: #selectAllUnitTests;
		add: 'Select subclasses' selector: #selectSubclasses;
		add: 'Select inversion' selector: #selectInverseClasses;
		add: 'Select none' selector: #selectNoClasses;
		add: 'Refresh' selector: #updateClasses;
		addLine;
		add: 'Filter...' selector: #filterClasses;
		addLine;
		add: 'Run selected' selector: #runAll;
		yourself.