add/remove
addFalsePositive: aCritic forRule: aRule

	(falsePositives includesKey: aRule) 
		ifFalse: [ falsePositives at: aRule put: IdentitySet new ].
	(falsePositives at:aRule) add: aCritic. 
	self updateBrowser 	