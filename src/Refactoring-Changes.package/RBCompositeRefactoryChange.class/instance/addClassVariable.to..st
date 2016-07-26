refactory-changes
addClassVariable: variableName to: aClass 
	^ self addChange: (RBAddClassVariableChange add: variableName to: aClass)