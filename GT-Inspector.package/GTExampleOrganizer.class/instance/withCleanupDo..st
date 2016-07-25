OBSOLETE
withCleanupDo: aBlock
	self deprecated: 'use #withoutExceptionsDo: instead'.
	self withoutExceptionsDo: aBlock