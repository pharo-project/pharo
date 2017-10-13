*System-Support
methodsReferencingClasses: aCollectionOfClasses
	^ self methods select: [ :meth | meth referencedClasses includesAny: aCollectionOfClasses ]