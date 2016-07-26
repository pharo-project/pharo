type of methods
asActive

	"Sets the receiver as active object, which will allow itself to retrieve its data from the compiled method"
	self annotationNamed: self class statusKey put: #active