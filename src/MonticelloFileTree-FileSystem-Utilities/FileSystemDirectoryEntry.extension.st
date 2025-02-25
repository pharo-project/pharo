Extension { #name : 'FileSystemDirectoryEntry' }

{ #category : '*MonticelloFileTree-FileSystem-Utilities' }
FileSystemDirectoryEntry >> name [
    ^ self basename
]

{ #category : '*MonticelloFileTree-FileSystem-Utilities' }
FileSystemDirectoryEntry >> readStreamDo: aBlock [
    ^ reference readStreamDo: aBlock
]
