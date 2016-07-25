instance creation
forClassNamed: aClassName
	^ self new
		registeredClassName: aClassName;
		yourself