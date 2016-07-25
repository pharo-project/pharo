annotations
subclasses
	"Retrieves the direct subclasses of the receiver.
	This value is kept as an annotation"

	^self annotationNamed: self class subclassesKey
		ifAbsentPut:[ OrderedCollection new ]