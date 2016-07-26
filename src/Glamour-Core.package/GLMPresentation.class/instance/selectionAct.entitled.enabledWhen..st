scripting actions
selectionAct: aBlock entitled: aString enabledWhen: conditionBlock
	 
	self 
		addSelectionAction: 
			((GLMGenericAction new) 
				action: aBlock; 
				title: aString;
				enabledCondition: conditionBlock; 
				yourself)