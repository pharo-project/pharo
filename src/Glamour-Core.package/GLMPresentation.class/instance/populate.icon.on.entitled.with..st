scripting actions
populate: aPortSymbol icon: anIcon on: aCharacter entitled: aString with: aBlock 
	 
	self 
		addAction: 
			((GLMGenericAction new) 
				action: 
						((GLMPortUpdater new) 
								portSymbol: aPortSymbol; 
								valueBlock: aBlock; 
								yourself); 
				shortcut: aCharacter; 
				icon: anIcon;
				title: aString;
				yourself)