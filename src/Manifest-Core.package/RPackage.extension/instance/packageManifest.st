*Manifest-Core
packageManifest
	^ self classes
		detect: [ :each | each isManifest ]
		ifNone: [ TheManifestBuilder new createManifestNamed: name]