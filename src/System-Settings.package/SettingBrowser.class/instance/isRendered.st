user interface
isRendered
	^ self dependents anySatisfy: [ :dependent | dependent isSystemWindow and: [ dependent owner isNotNil ] ]