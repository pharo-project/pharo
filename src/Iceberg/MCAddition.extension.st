Extension { #name : #MCAddition }

{ #category : #'*Iceberg-Changes' }
MCAddition >> myVersionOn: aDiff [

	^ aDiff versionFromDefinition: definition
]

{ #category : #'*Iceberg-Changes' }
MCAddition >> theirVersionOn: aDiff [

	^ nil
]
