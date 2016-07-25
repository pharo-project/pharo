scripting actions
act: aBlock entitled: aString 
	 
	self 
		addAction: 
			((GLMGenericAction new) 
				action: aBlock; 
				title: aString; 
				yourself)