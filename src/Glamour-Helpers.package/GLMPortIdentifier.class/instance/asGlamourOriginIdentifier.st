accessing
asGlamourOriginIdentifier
	self portName isNil ifTrue: [self portName: self class defaultOriginPortName].
	^ self 