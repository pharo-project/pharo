composing
composedWithLocal: aTransformation
	"Return the composition of the receiver and the local transformation passed in.
	A 'local' transformation is defined as a transformation that takes place
	between the receiver (the 'global') transformation and any 'local' point
	computations, e.g., for the methods

		globalPointToLocal: globalPoint
			globalPoint -> globalTransform -> localTransform -> locaPoint

		localPointToGlobal: localPoint
			localPoint -> localTransform -> globalTransform -> globalPoint

		"
	self isIdentity ifTrue:[^ aTransformation].
	aTransformation isIdentity ifTrue:[^ self].
	^ CompositeTransform new globalTransform: self
							localTransform: aTransformation