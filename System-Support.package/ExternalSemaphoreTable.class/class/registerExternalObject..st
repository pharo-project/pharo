accessing
registerExternalObject: anObject
	^ ProtectAdd critical: [self privateRegisterExternalObject: anObject]
