refactory-changes
removeInstanceVariable: variableName from: aClass 
	^ self addChange: (RBRemoveInstanceVariableChange remove: variableName from: aClass)