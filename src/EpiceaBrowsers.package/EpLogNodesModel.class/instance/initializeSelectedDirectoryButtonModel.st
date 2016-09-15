initialization
initializeSelectedDirectoryButtonModel
	
	selectDirectoryButtonModel 
		help: 'Select directory';
		icon: (Smalltalk ui icons iconNamed: #openIcon);
		action: [ self selectDirectory ]
