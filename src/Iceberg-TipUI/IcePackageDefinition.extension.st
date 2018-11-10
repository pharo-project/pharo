Extension { #name : #IcePackageDefinition }

{ #category : #'*Iceberg-TipUI' }
IcePackageDefinition >> canBeBrowsed [
	
	^ RPackage organizer includesPackageNamed: name
]

{ #category : #'*Iceberg-TipUI' }
IcePackageDefinition >> canBrowseReferences [
	
	^ false
]

{ #category : #'*Iceberg-TipUI' }
IcePackageDefinition >> icon [
	^ self iconNamed: #dirtyMonticelloPackage
]
