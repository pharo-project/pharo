Extension { #name : #FileSystem }

{ #category : #'*Filesystem-Zip' }
FileSystem class >> zip: aReference [
	^ self store: (ZipStore reference: aReference)
]
