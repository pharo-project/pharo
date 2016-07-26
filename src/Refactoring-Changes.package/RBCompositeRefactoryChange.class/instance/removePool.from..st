refactory-changes
removePool: aPoolVariable from: aClass 
	^ self addChange: (RBRemovePoolVariableChange remove: aPoolVariable from: aClass)