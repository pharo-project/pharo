accessing
uniqueInstance
	self resetIfInvalid.
	^ default ifNil: [default := self new directory: self defaultDirectory]