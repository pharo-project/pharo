scripting actions
spawn: aBlock icon: anIcon entitled: aString
	self 
		addAction: 
			((GLMSpawnBrowserAction new) 
				action: aBlock; 
				icon: anIcon;
				title: aString;
				yourself)