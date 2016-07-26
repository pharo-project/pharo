protocol
invalidateCacheEntryFor: aMethod
	self removeAllFromMethodsIconsCache: aMethod.
	self updateMethodList