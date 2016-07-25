testing
currentPackageLoaded: loadedBlock notLoaded: notLoadedBlock using: anMCLoader
	"Use currentVersionInfoFor: because it involves the loader and returns versionInfo for a planned load 
	 (atomic loaders) or currently loaded package"

	| wcName vis |
	vis := anMCLoader ancestorsFor: self.
	vis notNil
		ifTrue: [ 
			self getFile == nil
				ifTrue: [ ^ loadedBlock value: #() value: self file ].
			^ loadedBlock value: vis value: self file ].
	^ notLoadedBlock value