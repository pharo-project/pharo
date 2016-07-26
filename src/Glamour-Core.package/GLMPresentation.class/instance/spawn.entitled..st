scripting actions
spawn: aBlock entitled: aString 
	"These are actions that return a glamour browser to open at the end."
	self 
		addAction: 
			((GLMSpawnBrowserAction new) 
				action: aBlock; 
				title: aString; 
				yourself)