refactory-changes
addPool: aPoolVariable to: aClass 
	^ self addChange: (RBRefactoryChangeManager changeFactory addPoolVariable: aPoolVariable to: aClass)