initialization
initializeButtons
	buttonOpen := self newButton
		help: 'Open the package dependencies analysis';
		label: 'Browse dependencies';
		icon: (self iconNamed: #packageIcon).
	buttonAddPackage := self newButton
		help: 'Add package to browse';
		label: 'Add package';
		icon: (self iconNamed: #addIcon).
	buttonAddAll := self newButton
		help: 'Add all packages';
		label: 'Add all';
		icon: (self iconNamed: #addIcon).
	buttonRemovePackage := self newButton
		help: 'Remove package to browse';
		label: 'Remove package';
		icon: (self icon: #deleteIcon).
	buttonRemoveAll := self newButton
		help: 'Remove all selected packages';
		label: 'Remove all';
		icon: (self icon: #deleteIcon)