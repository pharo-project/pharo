installing package
installPackage: aHEPackage
	| newTraits newClasses |
	RPackageOrganizer default registerPackageNamed: aHEPackage packageName.

	newTraits := aHEPackage traits collect: [ :exportedTrait | self buildTrait: exportedTrait ].

	aHEPackage traits with: newTraits do: [ :exportedTrait :newTrait | self installMethods: exportedTrait into: newTrait ].

	newClasses := aHEPackage classes
		collect: [ :exportedClass | 
			| aClass |
			('[Hermes] Creating class ' , exportedClass className) logCr.
			
			aClass := self build: exportedClass.

			('[Hermes] Installing class ' , exportedClass className) logCr.

			self installClass: aClass in: exportedClass category.
			aClass ].

	aHEPackage classes with: newClasses do: [ :exportedClass :newClass | self installMethods: exportedClass into: newClass ].

	aHEPackage extensionMethods do: [ :e | self installExtensionMethod: e ]