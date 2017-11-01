Extension { #name : #FileSystemDirectoryEntry }

{ #category : #'*monticellofiletree-filesystem-utilities' }
FileSystemDirectoryEntry >> name [
    ^ self basename
]

{ #category : #'*monticellofiletree-filesystem-utilities' }
FileSystemDirectoryEntry >> readStreamDo: aBlock [
    ^ reference readStreamDo: aBlock
]
