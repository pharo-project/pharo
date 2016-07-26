private registration
unregisterExtendingPackage: aPackage forClass: aClass
	| extendingPackageForClass |
	
	extendingPackageForClass := classExtendingPackagesMapping 
		at: aClass theNonMetaClass name 
		ifAbsent: [ nil ].
	extendingPackageForClass ifNotNil: [ 
		extendingPackageForClass 
			remove: aPackage 
			ifAbsent: [] "not happy with this one" ]