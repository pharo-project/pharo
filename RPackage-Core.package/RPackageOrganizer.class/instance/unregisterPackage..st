public registration
unregisterPackage: aPackage
	"Unregister the specified package from the list of registered packages. Raise the announcement RPackageUnregistered."

	self basicUnregisterPackage: aPackage.
	aPackage extendedClasses 
		do: [ :extendedClass | self unregisterExtendingPackage: aPackage forClass: extendedClass].
	aPackage definedClasses 
		do: [ :definedClass | self unregisterPackage: aPackage forClass: definedClass].
	self announcer announce: (RPackageUnregistered to: aPackage).
	
	^ aPackage