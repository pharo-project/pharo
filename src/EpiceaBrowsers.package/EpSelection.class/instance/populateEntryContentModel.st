populating browser
populateEntryContentModel

	self items 
		ifEmpty: [ 
			browser entryContentModel 
				leftText: '';
				rightText: '';
				showOnlyDestination: true ]
		ifNotEmpty: [ :someItems | 
			| item |
			item := someItems anyOne.

			"FIX"			
			browser entryContentModel 
				showOnlyDestination: (item oldStateString isEmptyOrNil);
				showOnlySource: (item newStateString isEmptyOrNil);
				leftText: item oldStateString;
				rightText: item newStateString ]
