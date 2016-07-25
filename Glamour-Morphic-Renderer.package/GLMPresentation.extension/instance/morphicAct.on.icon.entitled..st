*glamour-morphic-renderer
morphicAct: aBlock on: aCharacter icon: anIcon entitled: aString 
	 
	self 
		addAction: 
			((GLMMorphicAction new) 
				action: aBlock; 
				shortcut: aCharacter; 
				icon: anIcon;
				title: aString; 
				yourself)