initialization
initializeCreateLogButtonModel

	createLogButtonModel
		icon: (self iconNamed: #addIcon);
		help: 'Start recording into new Log';
		action: [ self createNewActiveLog ].