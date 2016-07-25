generic parent api
parentName
	"Retrieves the name of the class defining the receiver. Its value is kept as an annotation"

	^ self annotationNamed: self class classNameKey