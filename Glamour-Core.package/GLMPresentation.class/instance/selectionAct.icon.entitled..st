scripting actions
selectionAct: aBlock icon: anIcon entitled: aString
	self 
		addSelectionAction: 
			((GLMGenericAction new) 
				action: aBlock; 
				icon: anIcon;
				title: aString;
				yourself)