accessing
asGlamourTargetIdentifier
	self portName isNil ifTrue: [self portName: self class defaultTargetPortName].
	^ self 