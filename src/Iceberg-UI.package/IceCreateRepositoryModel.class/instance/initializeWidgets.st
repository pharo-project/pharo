initialization
initializeWidgets
	localDirectoryLocation := self instantiate: IceDirectoryModel.
	localDirectoryLocation 
		label: 'Local directory';
		chooseTitle: 'Choose local repository';
		location: self defaultLocation.
	
	subdirectoryLabel := self newLabel label: 'Code subdirectory'.
	subdirectory := self newTextInput autoAccept: true.
	
	createButton := self newButton
		label: self class buttonLabel;
		yourself.
	
	self focusOrder 
		add: localDirectoryLocation;
		add: subdirectory;
		add: createButton