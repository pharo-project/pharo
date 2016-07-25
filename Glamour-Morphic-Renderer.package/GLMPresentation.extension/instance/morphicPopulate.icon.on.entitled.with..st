*glamour-morphic-renderer
morphicPopulate: aPortSymbol icon: anIcon on: aCharacter entitled: aString with: aBlock 
	 
	self 
		addAction: 
			((GLMMorphicAction new) 
				action: 
						((GLMPortUpdater new) 
								portSymbol: aPortSymbol; 
								valueBlock: aBlock; 
								yourself); 
				shortcut: aCharacter; 
				icon: anIcon;
				title: aString;
				yourself)