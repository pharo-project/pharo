testing
includesClassNamed: className
	"Returns true if the receiver includes the className in the defined classes"

	self flag: 'can a metaclass be stored without its nonMetaClass? Check this'.
	^self includesElementNamed: (self theNonMetaClassNameOf: className) in: self definedClasses.