Extension { #name : #FileStream }

{ #category : #'*FileSystem-Core' }
FileStream class >> onHandle: aFileSystemHandle [
	^ self concreteStream new
		open: aFileSystemHandle fullName
		forWrite: aFileSystemHandle isWritable
]
