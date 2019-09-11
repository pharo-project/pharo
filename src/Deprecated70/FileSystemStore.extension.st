Extension { #name : #FileSystemStore }

{ #category : #'*Deprecated70' }
FileSystemStore >> entryFromNode: aNode fileSystem: aFilesystem path: aPath [

	self 
		deprecated: 'Use FileSystemDirectoryEntry class>>fileSystem:path:'
		on: 	'30 Apr 2017' 
		in: 'Pharo-7.0'.
	^ FileSystemDirectoryEntry
		fileSystem: aFilesystem
		path: aPath
]
