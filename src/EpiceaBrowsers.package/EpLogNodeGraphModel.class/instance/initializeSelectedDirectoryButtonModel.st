initialization
initializeSelectedDirectoryButtonModel
	
	selectDirectoryButtonModel 
		help: 'Select directory to browse logs (i.e. files with ombu extension)';
		icon: (self iconNamed: #openIcon);
		action: [ self selectDirectory ]
