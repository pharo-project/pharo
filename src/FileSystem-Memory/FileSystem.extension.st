Extension { #name : #FileSystem }

{ #category : #'*FileSystem-Memory' }
FileSystem >> isMemoryFileSystem [
	^ store isMemoryFileSystem
]

{ #category : #'*FileSystem-Memory' }
FileSystem class >> memory [
	^ MemoryStore currentFileSystem
]
