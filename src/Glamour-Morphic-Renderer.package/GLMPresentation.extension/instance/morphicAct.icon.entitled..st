*glamour-morphic-renderer
morphicAct: aBlock icon: anIcon entitled: aString 
	 
	self addAction: 
			((GLMMorphicAction new) 
				action: aBlock; 
				title: aString; 
				icon: anIcon;
				yourself)