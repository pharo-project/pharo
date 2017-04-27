initialization
initializeSelectedDirectoryButtonModel
	
	selectDirectoryButtonModel 
		help: 'Select directory to browse logs (i.e. files with ombu extension)';
		icon: (self iconNamed: #openIcon);
		label: 'Directory';
		action: [ self selectDirectory ]
