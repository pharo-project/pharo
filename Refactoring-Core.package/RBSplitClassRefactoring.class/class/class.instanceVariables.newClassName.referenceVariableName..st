instance creation
class: class instanceVariables: instVars newClassName: className referenceVariableName: newVariable 
	^(self new)
		class: class
			instanceVariables: instVars
			newClassName: className
			referenceVariableName: newVariable;
		yourself