annotations
definitionSource: aString
	"Sets the definition template of the receiver -> aString.
	It's stored as an annotation"
	
	self annotationNamed: self class definitionSourceKey
		put: aString