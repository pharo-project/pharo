managing accessors
removeAccessor: aSelector
	self package removeMethod: (self
		compiledMethodAt: aSelector
		ifAbsent: [ ^ self ]).
		
	^ super removeAccessor: aSelector