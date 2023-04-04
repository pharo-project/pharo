Extension { #name : #RPackageOrganizer }

{ #category : #'*CodeExport' }
RPackageOrganizer >> fileOut [

	^ self systemOrganizer fileOut
]

{ #category : #'*CodeExport' }
RPackageOrganizer >> fileOutCategory: category [

	^ self systemOrganizer fileOutCategory: category
]

{ #category : #'*CodeExport' }
RPackageOrganizer >> fileOutCategory: category on: aFileStream [

	^ self systemOrganizer fileOutCategory: category on: aFileStream
]

{ #category : #'*CodeExport' }
RPackageOrganizer >> fileOutCategory: category on: aFileStream initializing: aBool [

	^ self systemOrganizer fileOutCategory: category on: aFileStream initializing: aBool
]
