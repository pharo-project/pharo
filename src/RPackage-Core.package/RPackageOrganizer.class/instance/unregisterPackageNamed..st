public registration
unregisterPackageNamed: symbol
	"Unregister the specified package from the list of registered packages. Raise the announcement RPackageUnregistered."
	
	| aPackage |
	aPackage := packages at: symbol asSymbol ifAbsent: [ nil].
	aPackage 
		ifNotNil: [self unregisterPackage: aPackage].
	^ aPackage