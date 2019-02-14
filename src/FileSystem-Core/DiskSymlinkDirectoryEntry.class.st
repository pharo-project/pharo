"
DiskSymlinkDirectoryEntry provides the same information as DiskDirectoryEntry, but for the symbolic link (and not the target file).

See DiskDirectoryEntry for more information.
"
Class {
	#name : #DiskSymlinkDirectoryEntry,
	#superclass : #DiskDirectoryEntry,
	#category : #'FileSystem-Core-Public'
}

{ #category : #'private - accessing' }
DiskSymlinkDirectoryEntry >> statAttributeMask [
	"Answer the mask used to retrieve lstat() attributes"

	^2r101
]

{ #category : #accessing }
DiskSymlinkDirectoryEntry >> targetPath [
	"Answer the Path pointing to the real file.
	If the receiver is a symbolic link, answer a file reference of the target.
	If not, answer the receivers file"

	| fileString |
	fileString := self statAttributes first.
	^fileString
		ifNil: [ reference path ]
		ifNotNil: [ fileString asPath ].
]
