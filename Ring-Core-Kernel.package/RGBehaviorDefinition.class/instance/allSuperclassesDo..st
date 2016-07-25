accessing class hierarchy
allSuperclassesDo: aBlock
	"Evaluate the argument, aBlock, for each of the receiver's superclasses."

	self superclass isNil
		ifTrue: [ ^ self ].
	aBlock value: superclass.
	superclass allSuperclassesDo: aBlock