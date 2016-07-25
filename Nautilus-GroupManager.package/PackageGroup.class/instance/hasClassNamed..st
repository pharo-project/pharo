testing
hasClassNamed: aClassName
	^ packages anySatisfy: [ :package | package classes anySatisfy: [ :class | class name = aClassName ] ]