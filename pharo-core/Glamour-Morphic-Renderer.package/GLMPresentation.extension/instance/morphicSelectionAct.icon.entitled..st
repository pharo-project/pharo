*glamour-morphic-renderer
morphicSelectionAct: aBlock icon: anIcon entitled: aString
	self 
		addSelectionAction: 
			((GLMMorphicAction new) 
				action: aBlock; 
				icon: anIcon;
				title: aString;
				yourself)