refactory-changes
removeClass: aClass 
	^ self addChange: (RBRefactoryChangeManager changeFactory removeClassNamed: aClass name)