Extension { #name : 'Behavior' }

{ #category : '*UnifiedFFI' }
Behavior >> basicNewPinned [

	<primitive: 598 error: ec>

	^ self basicNew
		pinInMemory;
		yourself
]

{ #category : '*UnifiedFFI' }
Behavior >> basicNewPinned: requestedSize [

	<primitive: 599 error: ec>

	^ (self basicNew: requestedSize)
		pinInMemory;
		yourself
]

{ #category : '*UnifiedFFI' }
Behavior >> ffiBindingOf: aName [
	"answer a binding for a type name,
	by default use smalltalk name bindings"
	^ self bindingOf: aName
]

{ #category : '*UnifiedFFI' }
Behavior >> ffiInstVarArgument: argName generator: generator [
	"Load the instance variable with given name"

	(self allInstVarNames includes: argName) ifFalse: [ ^ nil ].
	^ FFIInstVarArgument new
		argName: argName;
		yourself
]

{ #category : '*UnifiedFFI' }
Behavior >> newPinned [

	^ self basicNewPinned initialize
]

{ #category : '*UnifiedFFI' }
Behavior >> newPinned: sizeRequested [

	^ (self basicNewPinned: sizeRequested) initialize
]
