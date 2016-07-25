scripting actions
populate: aPortSymbol on: aCharacter entitled: aString with: aBlock 
	 
	self 
		addAction: 
			((GLMGenericAction new) 
				action: 
						((GLMPortUpdater new) 
								portSymbol: aPortSymbol; 
								valueBlock: aBlock; 
								yourself); 
				shortcut: aCharacter; 
				title: aString;
				yourself)