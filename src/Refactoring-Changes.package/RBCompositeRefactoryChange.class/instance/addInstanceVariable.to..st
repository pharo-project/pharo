refactory-changes
addInstanceVariable: variableName to: aClass 
	^ self addChange: (RBRefactoryChangeManager changeFactory addInstanceVariable: variableName to: aClass)