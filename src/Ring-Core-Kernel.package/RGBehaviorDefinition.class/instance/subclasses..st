annotations
subclasses: aCollection
	"Stores direct subclasses of the receiver as an annotation"

	self annotationNamed: self class subclassesKey
		ifAbsentPut:[ aCollection ]