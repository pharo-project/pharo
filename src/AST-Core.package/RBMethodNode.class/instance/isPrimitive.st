testing
isPrimitive
	^ self pragmas anySatisfy: [ :each | each isPrimitive ]