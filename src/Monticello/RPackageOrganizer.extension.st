Extension { #name : #RPackageOrganizer }

{ #category : #'*Monticello-RPackage' }
RPackageOrganizer >> isDefinedAsPackageOrSubPackageInMC: aSymbol [
	"a category has been added. "

	^ MCWorkingCopy allWorkingCopies anySatisfy: [ :manager | manager packageName isCategoryOf: aSymbol ]
]
