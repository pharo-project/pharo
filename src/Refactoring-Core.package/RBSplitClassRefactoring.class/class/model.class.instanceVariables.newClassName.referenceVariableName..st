instance creation
model: aRBSmalltalk class: class instanceVariables: instVars newClassName: className referenceVariableName: newVariable 
	^(self new)
		model: aRBSmalltalk;
		class: class
			instanceVariables: instVars
			newClassName: className
			referenceVariableName: newVariable;
		yourself