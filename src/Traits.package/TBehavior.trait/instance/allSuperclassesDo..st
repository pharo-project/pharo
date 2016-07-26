enumerating
allSuperclassesDo: aBlock 
	"Evaluate the argument, aBlock, for each of the receiver's superclasses."

	self superclass == nil
		ifFalse: [aBlock value: self superclass.
				self superclass allSuperclassesDo: aBlock]