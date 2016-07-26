actions
addMethodReference: aMethodReference.

	self addMethodReferenceSilently: aMethodReference.
	self checkSize.
	self updateView