Extension { #name : #FileSystem }

{ #category : #'*filesystem-zip' }
FileSystem class >> zip: aReference [
	^ self store: (ZipStore reference: aReference)
]
