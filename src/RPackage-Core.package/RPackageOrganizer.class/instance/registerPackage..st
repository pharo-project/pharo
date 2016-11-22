public registration
registerPackage: aPackage
	"A new package is now available and declared in the receiver. "
	
	self validateCanBeAddedPackage: aPackage.
	
	self basicRegisterPackage: aPackage.
	aPackage extendedClasses 
		do: [ :extendedClass | self registerExtendingPackage: aPackage forClass: extendedClass].
	aPackage definedClasses 
		do: [ :definedClass | self registerPackage: aPackage forClass: definedClass].
		
	SystemAnnouncer uniqueInstance announce: (RPackageRegistered to: aPackage).
	
	^ aPackage