initialization
initializeSelectedDirectoryButtonModel
	
	selectDirectoryButtonModel 
		help: 'Select directory';
		icon: (self iconNamed: #openIcon);
		action: [ self selectDirectory ]
