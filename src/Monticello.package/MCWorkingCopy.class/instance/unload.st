operations
unload
	"Unloads mcpackage, rpackage, classes and method extensions from this working copy"
	| postUnloadAction |
	
	postUnloadAction := [].
	self package correspondingRPackage ifNotNil: [ :anRPackage |
		anRPackage packageManifestOrNil ifNotNil: [ :manifest |
			postUnloadAction := manifest postUnloadAction.
			manifest preUnload ]].
	MCPackageLoader unloadPackage: self package.		
	package packageSet unregister.
	self unregister.
	postUnloadAction value.
	
