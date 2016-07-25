accessing
unregisterExternalObject: anObject
	ProtectRemove critical: [self privateUnregisterExternalObject: anObject]
