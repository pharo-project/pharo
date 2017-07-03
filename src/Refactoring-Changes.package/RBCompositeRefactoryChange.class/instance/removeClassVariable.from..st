refactory-changes
removeClassVariable: variableName from: aClass 
	^ self addChange: (RBRefactoryChangeManager changeFactory removeClassVariable: variableName from: aClass)