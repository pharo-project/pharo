versions
fileNameForPackageNamed: name

	| versionInfos |
	versionInfos := self localRepository directory files
		select: [ :fileName | (fileName basename copyUpToLast: $-) = name ].
	
	versionInfos
		ifEmpty: [ self error: 'No versionInfo for package: ', name ].
	versionInfos size > 1
		ifTrue: [ self error: 'More than one versionInfo for package "', name , '": ', versionInfos asArray asString ].
	
	^ versionInfos anyOne basename