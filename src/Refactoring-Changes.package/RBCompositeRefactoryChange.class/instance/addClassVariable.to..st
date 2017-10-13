refactory-changes
addClassVariable: variableName to: aClass 
	^ self addChange: (RBRefactoryChangeManager changeFactory addClassVariable: variableName to: aClass)