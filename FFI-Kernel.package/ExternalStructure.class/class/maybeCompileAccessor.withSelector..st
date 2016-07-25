compiling
maybeCompileAccessor: aString withSelector: selector
	(self compiledMethodAt: selector ifAbsent: []) ifNotNil:
		[:existingMethod|
		existingMethod getSourceFromFile asString = aString ifTrue:
			[^self]].
	self compile: aString classified: #accessing