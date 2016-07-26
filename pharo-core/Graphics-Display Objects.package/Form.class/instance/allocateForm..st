initialization
allocateForm: extentPoint
	"Allocate a new form which is similar to the receiver and can be used for accelerated blts"
	^Form extent: extentPoint depth: self nativeDepth