scripting actions
act: aBlock icon: anIcon entitled: aString
	self 
		addAction: 
			((GLMGenericAction new) 
				action: aBlock; 
				icon: anIcon;
				title: aString;
				yourself)