initialization
initializeTextInputFieldModel

	|critics|
	textInputFieldModel 
		autoAccept: true;
		ghostText: 'Search field';
		text: ' ';
		removeEntryCompletion.	
	textInputFieldModel acceptBlock: [ :text | |matchText|
		critics :=cache criticsOf: rule.
		matchText := '*', text, '*'.
		criticsModel items: (text isEmpty 
			ifTrue: [ critics ]
			ifFalse: [ critics select: [ :critic |
				matchText match: (self formatCritic: critic)]]).
		criticsModel updateList ]
