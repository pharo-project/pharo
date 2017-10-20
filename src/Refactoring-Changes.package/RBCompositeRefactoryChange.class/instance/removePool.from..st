refactory-changes
removePool: aPoolVariable from: aClass 
	^ self addChange: (RBRefactoryChangeManager changeFactory removePoolVariable: aPoolVariable from: aClass)