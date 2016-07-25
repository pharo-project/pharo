*glamour-morphic-renderer
morphicAct: aBlock on: aCharacter entitled: aString 
	 
	self 
		addAction: 
			((GLMMorphicAction new) 
				action: aBlock; 
				shortcut: aCharacter; 
				title: aString; 
				yourself)