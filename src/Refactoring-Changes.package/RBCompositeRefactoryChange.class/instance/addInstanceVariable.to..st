refactory-changes
addInstanceVariable: variableName to: aClass 
	^ self addChange: (RBAddInstanceVariableChange add: variableName to: aClass)