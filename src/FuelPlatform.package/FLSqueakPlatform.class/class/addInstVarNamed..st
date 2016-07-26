private-extensions-class
addInstVarNamed: aString
	<flExtensionOf: #Class>
	"Add the argument, aString, as one of the receiver's instance variables."
	^((Smalltalk globals at: #ClassBuilder) new)
		name: self name
		inEnvironment: self environment
		subclassOf: self superclass
		type: self typeOfClass
		instanceVariableNames: self instanceVariablesString, ' ', aString
		classVariableNames: self classVariablesString
		poolDictionaries: self sharedPoolsString
		category: self category
