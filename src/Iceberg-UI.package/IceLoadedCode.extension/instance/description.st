*Iceberg-UI
description
	^ self loadedVersions 
		ifEmpty: ['No package loaded']
		ifNotEmpty: [ self referenceCommit description ] 