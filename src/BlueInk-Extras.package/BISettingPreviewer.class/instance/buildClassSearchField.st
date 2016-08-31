item creation
buildClassSearchField
	^ self newTextInput
		ghostText: 'Enter the class name...';
		autoAccept: true;
		yourself