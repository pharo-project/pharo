running
cleaningResources
	Smalltalk globals
		at: #ManifestManifestResourcesTests
		ifPresent: [ :cl | 
			cl
				removeFromChanges;
				removeFromSystemUnlogged ]