initialization
initializeRefreshButtonModel

	refreshButtonModel 
		help: 'Refresh';
		icon: (self iconNamed: #smallUpdate);
		action: [ self refresh ]