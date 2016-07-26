*glamour-morphic-renderer
morphicSelectionAct: aBlock entitled: aString 
	 
	self addSelectionAction: 
			((GLMMorphicAction new) 
				action: aBlock; 
				title: aString; 
				yourself)