querying
hasChanges
	^ hasChanges
		or: [ initialTree isNil "It means that this is a new tree"
		or: [ self children anySatisfy: #hasChanges ]]
