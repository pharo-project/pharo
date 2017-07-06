refactory-changes
removeMethod: aSelector from: aClass 
	^ self addChange: (RBRefactoryChangeManager changeFactory removeMethod: aSelector from: aClass)