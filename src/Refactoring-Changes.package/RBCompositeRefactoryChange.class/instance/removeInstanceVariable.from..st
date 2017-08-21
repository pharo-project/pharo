refactory-changes
removeInstanceVariable: variableName from: aClass 
	^ self addChange: (RBRefactoryChangeManager changeFactory removeInstanceVariable: variableName from: aClass)