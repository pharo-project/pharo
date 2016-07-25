scripting actions
act: aBlock on: aCharacter entitled: aString 
	 
	self 
		addAction: 
			((GLMGenericAction new) 
				action: aBlock; 
				shortcut: aCharacter; 
				title: aString; 
				yourself)