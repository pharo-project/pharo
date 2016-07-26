running
setUp
	
	| cl |
	cl := Smalltalk globals at: #ManifestManifestResourcesTests ifAbsent: [ nil ].
	cl
		ifNotNil: [ 
			cl
				removeFromChanges;
				removeFromSystemUnlogged ].
