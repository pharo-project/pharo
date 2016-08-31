item creation
buildMethodSearchField
	^ self newTextInput
		ghostText: 'Enter the method selector...';
		whenTextIsAccepted: [ self whenSelectorAccepted ];
		yourself.