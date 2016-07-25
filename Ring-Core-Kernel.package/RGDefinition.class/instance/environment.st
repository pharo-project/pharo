accessing
environment
	"If the receiver has not namespace assigned the default is used"
	"self class environment = Smalltalk globals"

	^ self annotationNamed: #environment ifAbsent: [ self class environment ]