Extension { #name : #FileSystemDirectoryEntry }

{ #category : #'*Iceberg-Core' }
FileSystemDirectoryEntry >> packageName [ 
	^ self asFileReference basenameWithoutExtension

]
