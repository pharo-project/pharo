add/remove
removeToDo: aCritic forRule: aRule

	(toDos includesKey: aRule) 
		ifFalse: [^ self].
			
	(toDos at:aRule) remove: aCritic ifAbsent: [^ self].
	self updateBrowser
	