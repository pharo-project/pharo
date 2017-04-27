refreshing
refreshEntryContentModelWith: selectedItems

	selectedItems size = 1
		ifTrue: [
			| item |
			item := selectedItems anyOne.
			
			entryContentModel 
				showOnlyDestination: (item oldStateString isEmptyOrNil);
				showOnlySource: (item newStateString isEmptyOrNil);
				leftText: item oldStateString;
				rightText: item newStateString ]
		ifFalse: [
			entryContentModel
				leftText: '';
				rightText: '';
				showOnlyDestination: true ]
