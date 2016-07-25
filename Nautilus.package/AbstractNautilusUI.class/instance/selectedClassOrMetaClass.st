NOCompletion
selectedClassOrMetaClass
	self selectedCategory ifNil: [ self selectedMethod ifNil: [ ^ nil ] ].
	^ self selectedClass