fixing stream
unloadPackageNamed: aString
	"self new workingCopyFromPackageName: 'CollectionExtensions' "
	
	^ (self workingCopyFromPackageName: aString) unload
	