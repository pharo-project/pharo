initialization
initializeWidgets
	newButton := (self instantiate: ButtonModel)
		label: ' New project';
		icon: self class newIcon;
		help: 'Create a new project (configuration). Once created, double-click on the projet to edit it';
		yourself.
	
	loadButton := (self instantiate: ButtonModel)
		label: ' Load project';
		icon: self class loadIcon;
		help: 'Load an existing project (only its configuration) into the image';
		yourself.

	openButton := (self instantiate: ButtonModel)
		label: ' Open project';
		icon: self class openIcon;
		help: 'Open a new window on the selected project';
		yourself.
