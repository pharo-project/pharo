transforming
model
	^ model isNil 
		ifTrue: 
			[model := (RBClassModelFactory rbNamespace onEnvironment: self defaultEnvironment)
						name: self printString;
						yourself]
		ifFalse: [model]