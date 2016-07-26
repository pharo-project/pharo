refactory-changes
addPool: aPoolVariable to: aClass 
	^ self addChange: (RBAddPoolVariableChange add: aPoolVariable to: aClass)