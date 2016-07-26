extending
extendWord
	self hasFields ifTrue: [
		IncompatibleLayoutConflict new
			layout: self;
			subType: #word;
			signal ].
	^ WordLayout new