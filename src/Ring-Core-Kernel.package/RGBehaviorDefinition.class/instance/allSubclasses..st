accessing class hierarchy
allSubclasses: aCollection
	"Stores all the subclasses (direct and indirect) as an annotation"
	
	self annotationNamed: self class allSubclassesKey
		ifAbsentPut:[ aCollection ]