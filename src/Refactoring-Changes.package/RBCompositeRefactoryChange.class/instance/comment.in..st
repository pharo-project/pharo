refactory-changes
comment: aString in: aClass 
	^ self addChange: (RBRefactoryChangeManager changeFactory comment: aString in: aClass)