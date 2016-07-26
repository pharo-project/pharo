add/remove
addToDo: aCritic forRule: aRule

	(toDos includesKey: aRule) 
		ifFalse: [ toDos at:aRule put: IdentitySet new ].
	(toDos at:aRule) add: aCritic. 
	self updateBrowser