scripting actions
selectionAct: aBlock entitled: aString condition: conditionBlock
	 
	self 
		addSelectionAction: 
			((GLMGenericAction new) 
				action: aBlock; 
				title: aString;
				condition: conditionBlock; 
				yourself)