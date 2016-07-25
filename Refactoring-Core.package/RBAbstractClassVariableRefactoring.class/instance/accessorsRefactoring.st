private-accessing
accessorsRefactoring
	^accessorsRefactoring isNil 
		ifTrue: 
			[accessorsRefactoring := RBCreateAccessorsForVariableRefactoring 
						model: self model
						variable: variableName asString
						class: class
						classVariable: true]
		ifFalse: [accessorsRefactoring]