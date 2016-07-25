private registration
basicUnregisterPackageNamed: aSymbolName
	"Unregister the specified package from the list of registered packages. Raise the RPackageUnregistered announcement. This is a low level action. It does not unregister the back pointer from classes to packages or any other information managed by the organizer"
	
	packages
		removeKey: aSymbolName asSymbol
		ifAbsent: [ ]
	