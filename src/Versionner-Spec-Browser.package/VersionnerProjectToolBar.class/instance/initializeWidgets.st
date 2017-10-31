initialization
initializeWidgets
	updateButton := (self instantiate: ButtonPresenter)
		label: ' Save to development';
		icon: (self iconNamed: #smallSaveIcon);
		help: 'Save the current modifications into the development baseline. It will save your changes into the Configuration class but will not save your code in the Version Control System. To publish your changes, use the commit button.';
		yourself.

	releaseButton := (self instantiate: ButtonPresenter)
		label: ' Release version';
		icon: (self iconNamed: #smallPublishIcon);
		help: 'Make a release. It will create a version method for this release and a new development baseline. It will save your changes into the Configuration class but will not save your code in the Version Control System. To publish your changes, use the commit button.';
		enabled: false;
		yourself.
	
	commitButton := (self instantiate: ButtonPresenter)
		label: ' Commit the project';
		icon: (self iconNamed: #monticelloPackageIcon);
		help: 'Commit the configuration and all dirty packages of the project to the main repository of your Version Control system (e.g. Monticello, git)';
		yourself.
