scripting actions
act: aBlock on: aCharacter entitled: aString categorized: anotherString 
	 
	self 
		addAction: 
			((GLMGenericAction new) 
				action: aBlock; 
				shortcut: aCharacter; 
				title: aString; 
				category: anotherString;
				yourself)