Extension { #name : 'RPackageOrganizer' }

{ #category : '*Monticello-RPackage' }
RPackageOrganizer >> isDefinedAsPackageOrSubPackageInMC: aSymbol [
	"a category has been added. "

	^ MCWorkingCopy allWorkingCopies anySatisfy: [ :workingCopy | workingCopy packageName isCategoryOf: aSymbol ]
]
