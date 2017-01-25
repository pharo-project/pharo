accessing
versionInfo
	^ self ancestors 
		ifNotEmpty: [ :list | list first ] 
		ifEmpty: [ nil ]