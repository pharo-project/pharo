Extension { #name : 'TheManifestBuilder' }

{ #category : '*Deprecated12' }
TheManifestBuilder class >> ofPackageNamed: aPackageName [

	| builder |
	self deprecated: 'Call #ofPackage: instead because the only senders of this method is in NewTools and has the package directly.'.
	builder := self new.
	self allManifestClasses
		detect: [ :each | each package name = aPackageName ]
		ifFound: [ :manifestClass | builder manifestClass: manifestClass ]
		ifNone: [ builder createManifestNamed: aPackageName ].
	^ builder
]
