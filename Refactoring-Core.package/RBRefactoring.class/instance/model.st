transforming
model
	^model isNil 
		ifTrue: 
			[model := (RBNamespace onEnvironment: self defaultEnvironment)
						name: self printString;
						yourself]
		ifFalse: [model]