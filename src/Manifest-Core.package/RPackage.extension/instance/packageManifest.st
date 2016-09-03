*Manifest-Core
packageManifest
	^ self definedClasses
		detect: [ :each | each isManifest ]
		ifNone: [ TheManifestBuilder new createManifestNamed: name]