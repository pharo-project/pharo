scripting actions
populate: aPortSymbol on: aCharacter with: aBlock
	self addAction: 
			((GLMGenericAction new) 
				action: ((GLMPortUpdater new) 
								portSymbol: aPortSymbol; 
								valueBlock: aBlock; 
								yourself); 
				shortcut: aCharacter; 
				yourself)