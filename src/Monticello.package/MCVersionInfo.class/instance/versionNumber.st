accessing
versionNumber
	^ ((self name copyAfterLast: $-) copyAfter: $.) asInteger