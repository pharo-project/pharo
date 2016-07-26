*glamour-morphic-renderer
morphicSelectionAct: aBlock icon: anIcon on: aCharacter entitled: aString
	self 
		addSelectionAction: 
			((GLMMorphicAction new) 
				action: aBlock; 
				shortcut: aCharacter; 				
				icon: anIcon;
				title: aString;
				yourself)