Extension { #name : #MCRemoval }

{ #category : #'*Iceberg-Changes' }
MCRemoval >> myVersionOn: aDiff [

	^ nil
]

{ #category : #'*Iceberg-Changes' }
MCRemoval >> theirVersionOn: aDiff [

	^ aDiff versionFromDefinition: definition
]
