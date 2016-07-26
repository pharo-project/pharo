add/remove
removeCritic: aCritic forRule: aRule

	(critics includesKey: aRule) 
		ifFalse: [^ self].
	(critics at: aRule) remove: aCritic ifAbsent: [^ self].
	self updateBrowser
	