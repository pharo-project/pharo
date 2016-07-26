scripting actions
selectionAct: aBlock on: aCharacter entitled: aString 
	 
	self 
		addSelectionAction: 
			((GLMGenericAction new) 
				action: aBlock; 
				shortcut: aCharacter; 
				title: aString; 
				yourself)