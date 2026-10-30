"
I represent a zip file system entry for a file. Together with ZipFileSystemDirectory I take care of lazily accessing archive members.
"
Class {
	#name : 'ZipFileSystemFile',
	#superclass : 'MemoryFileSystemFile',
	#instVars : [
		'member'
	],
	#category : 'FileSystem-Zip-Base',
	#package : 'FileSystem-Zip',
	#tag : 'Base'
}

{ #category : 'accessing' }
ZipFileSystemFile >> bytes [
	"My member is read the first time I am used, then I hold the bytes, possibly written since"

	member ifNotNil: [
			bytes := self readFromMember.
			member := nil ].
	^ bytes
]

{ #category : 'accessing' }
ZipFileSystemFile >> member [
	^ member
]

{ #category : 'accessing' }
ZipFileSystemFile >> member: aZipArchiveMember [

	member := aZipArchiveMember.
	size := aZipArchiveMember uncompressedSize
]

{ #category : 'private' }
ZipFileSystemFile >> readFromMember [
	^ ByteArray
		new: self member uncompressedSize
		streamContents: [ :stream |
			self member extractTo: stream ]
]
