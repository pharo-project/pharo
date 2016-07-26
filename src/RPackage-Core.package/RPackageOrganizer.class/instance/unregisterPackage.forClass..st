private registration
unregisterPackage: aPackage forClass: aClass 
	"unregister the back pointer mapping from classes to packages."
		
	^classPackageMapping 
		removeKey: aClass theNonMetaClass name 
		ifAbsent: [ self reportExtraRemovalOf: aClass ]