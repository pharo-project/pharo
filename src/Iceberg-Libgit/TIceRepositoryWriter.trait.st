"
I indicate when a class is writer for iceberg 
"
Trait {
	#name : #TIceRepositoryWriter,
	#category : #'Iceberg-Libgit-Writing'
}

{ #category : #accessing }
TIceRepositoryWriter classSide >> id [
	^ self required
]

{ #category : #testing }
TIceRepositoryWriter classSide >> isDefault [
	"Indicates when this is default when create new repositories"
	^ false
]

{ #category : #testing }
TIceRepositoryWriter classSide >> isLegacyDefault [
	"Indicates when repository is default as legacy (when there is no properties file indicating 
	 a specific format type)"
	^ false
]
