scripting actions
selectionAct: aBlock entitled: aString 
	 
	self 
		addSelectionAction: 
			((GLMGenericAction new) 
				action: aBlock; 
				title: aString; 
				yourself)