scripting actions
selectionAct: aBlock icon: anIcon on: aCharacter entitled: aString 
	 
	self 
		addSelectionAction: 
			((GLMGenericAction new) 
				action: aBlock; 
				icon: anIcon;
				shortcut: aCharacter; 
				title: aString; 
				yourself)