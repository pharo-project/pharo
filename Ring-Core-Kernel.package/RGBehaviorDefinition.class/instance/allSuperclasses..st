accessing class hierarchy
allSuperclasses: aCollection
	"Stores all the superclasses (direct and indirect) as an annotation"
	
	self annotationNamed: self class allSuperclassesKey
		ifAbsentPut:[ aCollection ]