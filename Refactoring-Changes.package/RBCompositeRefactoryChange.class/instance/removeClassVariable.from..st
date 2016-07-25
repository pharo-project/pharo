refactory-changes
removeClassVariable: variableName from: aClass 
	^ self addChange: (RBRemoveClassVariableChange remove: variableName from: aClass)