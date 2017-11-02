"
I represent a zip file system entry for a file. Together with ZipFileSystemDirectory I take care of lazily accessing archive members.
"
Class {
	#name : #ZipFileSystemFile,
	#superclass : #MemoryFileSystemFile,
	#instVars : [
		'member'
	],
	#category : 'FileSystem-Zip'
}

{ #category : #accessing }
ZipFileSystemFile >> bytes [
	| uncompressed |
	uncompressed := self readFromMember.
	self updateSize: uncompressed size.
	^ uncompressed
]

{ #category : #accessing }
ZipFileSystemFile >> member [
	^ member
]

{ #category : #accessing }
ZipFileSystemFile >> member: aZipArchiveMember [
	member := aZipArchiveMember
]

{ #category : #private }
ZipFileSystemFile >> readFromMember [
	^ ByteArray 
		new: self member uncompressedSize
		streamContents: [ :stream |		
			self member extractTo: stream ]
]
