initialization
initializeTraitNamed: aClassName

	(self shouldInitializeClassNamed: aClassName) ifFalse: [ ^ self ].
	self doInitializeTraitNamed: aClassName.
	initializedClasses add: aClassName.