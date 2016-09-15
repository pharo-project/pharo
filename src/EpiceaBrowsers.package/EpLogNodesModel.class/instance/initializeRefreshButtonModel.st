initialization
initializeRefreshButtonModel

	refreshButtonModel 
		help: 'Refresh';
		icon: (Smalltalk ui icons iconNamed: #smallUpdate);
		action: [ self refresh ]