scripting actions
selectionPopulate: aPortSymbol entitled: aString with: aBlock 
	 
	self 
		addSelectionAction: 
			((GLMGenericAction new) 
				action: 
						((GLMPortUpdater new) 
								portSymbol: aPortSymbol; 
								valueBlock: aBlock; 
								yourself); 
				title: aString;
				yourself)