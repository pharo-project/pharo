accessing
versionFrom: aVersionReferenceString
	^ self loadVersionFromFileNamed: aVersionReferenceString , '.mcz'