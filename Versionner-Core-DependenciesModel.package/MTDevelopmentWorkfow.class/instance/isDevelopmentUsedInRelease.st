private
isDevelopmentUsedInRelease
	| devVersion allProjectVersions |
	
	devVersion := self developmentVersion.
	devVersion ifNil: [ ^ false ].
	(devVersion blessing = #baseline) ifFalse: [ ^false ].
	
	allProjectVersions := project configurationClass project versions.
	^ allProjectVersions 
		detect: [ :aVersion | aVersion importedVersions includes: devVersion versionString ]
		ifFound: [ true ]
		ifNone: [ false ]