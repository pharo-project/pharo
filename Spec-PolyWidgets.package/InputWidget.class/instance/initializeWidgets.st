initialization
initializeWidgets

	input 	:= self newTextInput.
	label	:= self newLabel.
			
	input
		ghostText: 'input';
		acceptBlock: [ self triggerOkAction ];
		entryCompletion: nil;
		acceptOnCR: true.
		
	label label: 'label'