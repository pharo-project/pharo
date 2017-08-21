enumerating
withAllSuperclassesDo: aBlock 
	"Evaluate the argument, aBlock, for each of the receiver's superclasses."
	aBlock value: self.
	self superclass == nil
		ifFalse: [self superclass withAllSuperclassesDo: aBlock]