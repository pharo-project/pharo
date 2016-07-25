private-class
privateSplitClassFor: aClass
	| class |
	class := aClass theNonMetaClass.
	^ RBSplitClassRefactoring
		model: environment
		class: class
		instanceVariables: (self chooseMultipleFrom: class instVarNames title: 'Select variables to extract:')
		newClassName: (self request: 'Enter new class name:')
		referenceVariableName: (self request: 'Enter new variable name where requests will be forwarded:')