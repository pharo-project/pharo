installing
isOverrideMethod
	"this oughta check the package"
	^ self isExtensionMethod and: [category endsWith: '-override']