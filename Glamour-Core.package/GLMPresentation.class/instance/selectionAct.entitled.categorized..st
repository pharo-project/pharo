scripting actions
selectionAct: aBlock entitled: aString categorized: anotherString
	 
	self 
		addSelectionAction: 
			((GLMGenericAction new) 
				action: aBlock; 
				title: aString; 
				category: anotherString;
				yourself)