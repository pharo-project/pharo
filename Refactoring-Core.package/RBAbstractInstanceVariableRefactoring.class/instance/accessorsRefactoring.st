private-accessing
accessorsRefactoring
	^accessorsRefactoring isNil 
		ifTrue: 
			[accessorsRefactoring := RBCreateAccessorsForVariableRefactoring 
						model: self model
						variable: variableName
						class: class
						classVariable: false]
		ifFalse: [accessorsRefactoring]