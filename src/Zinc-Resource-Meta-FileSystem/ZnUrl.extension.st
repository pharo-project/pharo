Extension { #name : #ZnUrl }

{ #category : #'*zinc-resource-meta-filesystem' }
ZnUrl >> asFileReference [
	"Convert the receiver into a new FileReference object.
	Note that for a FileReference a trailing slash is not relevant"

	| path |
	self assert: self scheme = #file description: 'Only a file:// URL can be converted to a FileReference'.
	self isSlash
		ifTrue: [ ^ FileSystem root ].
	path := self isDirectoryPath
		ifTrue: [ segments allButLast ]
		ifFalse: [ segments copy ].
	^ FileReference fileSystem: FileSystem disk path: (AbsolutePath withAll: path)
]
