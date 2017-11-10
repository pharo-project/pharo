Extension { #name : #Behavior }

{ #category : #'*UnifiedFFI' }
Behavior >> ffiBindingOf: aName [
	"answer a binding for a type name,
	by default use smalltalk name bindings"
	^ self bindingOf: aName
]

{ #category : #'*UnifiedFFI' }
Behavior >> ffiInstVarArgument: argName generator: generator [
	"Load the instance variable with given name" 

	(self allInstVarNames includes: argName) ifFalse: [ ^ nil ].
	^ FFIInstVarArgument new 
		argName: argName;
		yourself
]
