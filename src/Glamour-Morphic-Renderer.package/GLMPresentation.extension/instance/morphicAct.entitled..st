*glamour-morphic-renderer
morphicAct: aBlock entitled: aString 
	 
	self addAction: 
			((GLMMorphicAction new) 
				action: aBlock; 
				title: aString; 
				yourself)