transformations
composedWith: aTransform
	"Return a new transform that has the effect of transforming points first by the receiver and then by the argument."

	self isIdentity ifTrue: [^ aTransform].
	aTransform isIdentity ifTrue: [^ self].
	^ CompositeTransform new globalTransform: self
							localTransform: aTransform