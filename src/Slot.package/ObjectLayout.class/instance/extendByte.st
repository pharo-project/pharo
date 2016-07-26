extending
extendByte
	self hasFields ifTrue: [
		IncompatibleLayoutConflict new
			layout: self;
			subType: #byte;
			signal ].
	^ ByteLayout new