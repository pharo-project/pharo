scripting actions
selectionPopulate: aPortSymbol on: aCharacter entitled: aString with: aBlock 
	 
	self 
		addSelectionAction: 
			((GLMGenericAction new) 
				action: 
						((GLMPortUpdater new) 
								portSymbol: aPortSymbol; 
								valueBlock: aBlock; 
								yourself); 
				shortcut: aCharacter; 
				title: aString;
				yourself)