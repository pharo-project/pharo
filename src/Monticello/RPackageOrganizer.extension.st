Extension { #name : #RPackageOrganizer }

{ #category : #'*Monticello-RPackage' }
RPackageOrganizer class >> allManagers [

	^ MCWorkingCopy allManagers
]

{ #category : #'*Monticello-RPackage' }
RPackageOrganizer >> allManagers [

	^ self class allManagers 
]

{ #category : #'*Monticello-RPackage' }
RPackageOrganizer >> isDefinedAsPackageOrSubPackageInMC: aSymbol [
	"a category has been added. "

	^ self allManagers anySatisfy: [ :manager | manager packageName isCategoryOf: aSymbol ]
]
