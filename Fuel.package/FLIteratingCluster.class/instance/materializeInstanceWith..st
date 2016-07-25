serialize/materialize
materializeInstanceWith: aDecoder
	"Hook method that each subclass should customize for its special way of materializing"

	^ self subclassResponsibility