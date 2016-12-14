bootstrapping-bindings
bindingOfGlobal: aString

	self halt.
	^ bootstrappedBindingCache
		at: aString
		ifAbsentPut: [ 
			| association |
			association := (self classNamed: #Association) basicNew.
			association instanceVariableAtIndex: 1 put: self objectSpace nilObject.
			association instanceVariableAtIndex: 2 put: (self classNamed: aString).
			association ]