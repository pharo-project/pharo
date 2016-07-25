extending
extendSmallInteger
	self hasFields ifTrue: [
		IncompatibleLayoutConflict new
			layout: self;
			subType: #compiledMethod;
			signal ].
	^ ImmediateLayout new