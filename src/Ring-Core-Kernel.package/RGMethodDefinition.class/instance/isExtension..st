accessing
isExtension: aBoolean
	"Explicitily set that the receiver is an extension"
	
	self annotationNamed: self class isExtensionKey put: aBoolean 