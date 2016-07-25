scripting actions
selectionSpawn: aBlock entitled: aString 
	 
	"These are actions that return a glamour browser to open at the end."
	self 
		addSelectionAction: 
			((GLMSpawnBrowserAction new) 
				action: aBlock; 
				title: aString; 
				yourself)