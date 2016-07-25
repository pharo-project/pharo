add/remove
addCritic: aCritic forRule: aRule

	(critics includesKey: aRule) 
		ifFalse: [ critics at:aRule put: IdentitySet new ].
	(critics at:aRule) add: aCritic. 
	self updateBrowser