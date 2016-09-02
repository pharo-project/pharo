accessing
packageManifestOrNil
	^ self classes
		detect: [ :each | each isManifest ]
		ifNone: [ nil ]