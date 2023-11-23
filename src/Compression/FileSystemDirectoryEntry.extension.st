Extension { #name : 'FileSystemDirectoryEntry' }

{ #category : '*Compression' }
FileSystemDirectoryEntry >> addArchiveMemberAs: aName in: anArchive [

	self isDirectory
		ifTrue: [ anArchive addDirectory: self reference as: aName ]
		ifFalse: [ anArchive addFile: self reference as: aName ]
]
