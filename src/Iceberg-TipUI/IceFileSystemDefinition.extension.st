Extension { #name : #IceFileSystemDefinition }

{ #category : #'*Iceberg-TipUI' }
IceFileSystemDefinition >> canBeBrowsed [
	
	"It can always be browsed through the libgit file system"
	^ true
]

{ #category : #'*Iceberg-TipUI' }
IceFileSystemDefinition >> canBrowseReferences [
	
	"We do not track file references..."
	^ false
]
