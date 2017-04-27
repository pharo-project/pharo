initialization
initializeHiedraCheckBoxModel

	hiedraCheckBoxModel
		help: 'Analyze and display connections between log files.';
		label: 'Link logs';
		state: false;
		whenChangedDo: [ self refresh ].
