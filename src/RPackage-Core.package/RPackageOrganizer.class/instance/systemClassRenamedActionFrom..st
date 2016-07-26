system integration
systemClassRenamedActionFrom: ann
	"When a class is renamed, we update its package as well as the organizer"
	
	| newName oldName class rPackage extendingRPackages packageName |
	class := ann classAffected.
	packageName := ann category.
	oldName := ann oldName.
	newName := ann newName.
	rPackage := self packageOfClassNamed: oldName.
	extendingRPackages := self extendingPackagesOfClassNamed: oldName.
	
	rPackage updateDefinedClassNamed: oldName withNewName: newName.
	"we have to update all RPackages extending this class"
	extendingRPackages do: [:aRPackage | 
		aRPackage updateExtensionForClassNamed: oldName withNewName: newName].
		
	"we have to update the RPackageOrganizer.
	update the 'classPackageDictionary' to replace the key with the new class name"
	self unregisterPackage: rPackage forClassName: oldName.
	self registerPackage: rPackage forClassName: newName.	
		
	"update the 'classExtendingPackagesMapping' to replace the key with the new class name"
	extendingRPackages do: [:aRPackage |
		self unregisterExtendingPackage: aRPackage forClassName: oldName.
		self registerExtendingPackage:  aRPackage forClassName: newName   
		].
	self flag: #cyrille. "we will see that."
	"maybe we should remove also the key from the extendedPackages dictionary"
	"CD: This is what are doing the lines above I think"

	