initialization
initializeClassNamed: aClassName

	(self shouldInitializeClassNamed: aClassName) ifFalse: [ ^ self ].
		
	self doInitializeClassNamed: aClassName.
	initializedClasses add: aClassName.
	self postInitializeClassNamed: aClassName