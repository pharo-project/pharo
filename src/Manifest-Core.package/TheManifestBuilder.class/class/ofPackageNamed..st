instance creation
ofPackageNamed: aPackageName
	| builder |
	builder := self new.
	self allManifestClasses
		detect: [ :each | each category = aPackageName ]
		ifFound: [ :manifestClass | builder manifestClass: manifestClass ]
		ifNone: [ builder createManifestNamed: aPackageName ].
	^ builder