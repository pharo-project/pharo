accessing
versionFrom: aVersionReferenceString
	^ self dictionary detect: [ :version | version info name = aVersionReferenceString ]