Extension { #name : 'PackageOrganizer' }

{ #category : '*Monticello-RPackage' }
PackageOrganizer >> isDefinedAsPackageOrSubPackageInMC: aSymbol [
	"a category has been added. "

	^ MCWorkingCopy allWorkingCopies anySatisfy: [ :workingCopy | workingCopy packageName isCategoryOf: aSymbol ]
]
