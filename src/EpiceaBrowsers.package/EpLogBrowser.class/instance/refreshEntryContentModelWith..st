refreshing
refreshEntryContentModelWith: selectedItems

	selectedItems 
		ifEmpty: [ 
			entryContentModel 
				leftText: '';
				rightText: '';
				showOnlyDestination: true ]
		ifNotEmpty: [ :someItems | 
			| item |
			self flag: #todo. "FIX anyOne"
			item := someItems anyOne.

			entryContentModel 
				showOnlyDestination: (item oldStateString isEmptyOrNil);
				showOnlySource: (item newStateString isEmptyOrNil);
				leftText: item oldStateString;
				rightText: item newStateString ]
