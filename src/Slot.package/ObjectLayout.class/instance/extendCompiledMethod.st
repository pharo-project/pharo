extending
extendCompiledMethod
	self hasFields ifTrue: [
		IncompatibleLayoutConflict new
			layout: self;
			subType: #compiledMethod;
			signal ].
	^ CompiledMethodLayout new