Extension { #name : #FileSystem }

{ #category : #'*FileSystem-Memory' }
FileSystem >> isMemoryFileSystem [
	^ store isMemoryFileSystem
]

{ #category : #'*filesystem-memory' }
FileSystem class >> memory [
	^ self store: (MemoryStore new)
]
