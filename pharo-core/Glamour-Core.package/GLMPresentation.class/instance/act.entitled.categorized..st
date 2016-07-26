scripting actions
act: aBlock entitled: aString categorized: anotherString
	 
	self 
		addAction: 
			((GLMGenericAction new) 
				action: aBlock; 
				title: aString; 
				category: anotherString;
				yourself)