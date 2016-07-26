scripting actions
selectionAct: aBlock on: aCharacter 
	 
	self 
		addSelectionAction: 
			((GLMGenericAction new) 
				action: aBlock; 
				shortcut: aCharacter; 
				yourself)