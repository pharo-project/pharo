memory usage
sizeInMemory
	self class isImmediateClass
		ifTrue: [ ^ 0 ].
	^ self class isVariable
		ifTrue: [ self class byteSizeOfInstanceOfSize: self basicSize ]
		ifFalse: [ self class byteSizeOfInstance ]