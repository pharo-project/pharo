composing
composedWithGlobal: aTransformation
	"Return the composition of the receiver and the global transformation passed in.
	A 'global' transformation is defined as a transformation that takes place
	between the receiver (the 'local') transformation and any 'global' point
	computations, e.g., for the methods

		globalPointToLocal: globalPoint
			globalPoint -> globalTransform -> localTransform -> locaPoint

		localPointToGlobal: localPoint
			localPoint -> localTransform -> globalTransform -> globalPoint

		"
	^aTransformation composedWithLocal: self