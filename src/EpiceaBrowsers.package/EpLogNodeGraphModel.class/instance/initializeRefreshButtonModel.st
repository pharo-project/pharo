initialization
initializeRefreshButtonModel

	refreshButtonModel 
		help: 'Refresh logs from this directory';
		icon: (self iconNamed: #smallUpdate);
		label: 'Refresh';
		action: [ self refresh ]