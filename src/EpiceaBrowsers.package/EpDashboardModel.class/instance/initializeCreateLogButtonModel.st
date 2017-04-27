initialization
initializeCreateLogButtonModel

	createLogButtonModel
		icon: (self iconNamed: #addIcon);
		help: 'Start recording into new Log';
		label: 'New session';
		action: [ self createNewSessionLog ].