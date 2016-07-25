initialization
setBrowseButton

	browseButton
		state: false;
		enabled: false;
		label: 'Browse';
		help: 'Browse the working copy of the selected package';
		action: [ self browseWorkingCopy ]