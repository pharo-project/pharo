results
compiledMethod
	^ self itemClass
		compiledMethodAt: self selector
		ifAbsent: [self itemClass class
				compiledMethodAt: self selector
				ifAbsent: []]