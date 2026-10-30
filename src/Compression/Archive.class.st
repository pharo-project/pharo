"
This is the abstract superclass for file archives. Archives can be read from or written to files, and contain members that represent files and directories.
"
Class {
	#name : 'Archive',
	#superclass : 'Object',
	#instVars : [
		'members'
	],
	#category : 'Compression-Archives',
	#package : 'Compression',
	#tag : 'Archives'
}

{ #category : 'archive operations' }
Archive >> addDirectory: aFileReference [
	"Add a member for the directory aFileReference (not its contents) under its basename"

	^ self addDirectory: aFileReference as: aFileReference basename
]

{ #category : 'archive operations' }
Archive >> addDirectory: aFileReference as: anotherFileName [
	| newMember |
	newMember := self memberClass newFromDirectory: aFileReference localName: anotherFileName.
	self addMember: newMember.
	newMember localFileName: anotherFileName.
	^newMember
]

{ #category : 'archive operations' }
Archive >> addFile: aFileReference [
	"Add aFileReference under its basename. Use #addFile:as: or #addTree:relativeTo:match: to keep a path"

	^ self addFile: aFileReference as: aFileReference basename
]

{ #category : 'archive operations' }
Archive >> addFile: aFileReference as: anotherFileName [
	| newMember |
	newMember := self memberClass newFromFile: aFileReference.
	newMember localFileName: anotherFileName.
	self addMember: newMember.
	^newMember
]

{ #category : 'archive operations' }
Archive >> addMember: aMember [
	^members addLast: aMember
]

{ #category : 'archive operations' }
Archive >> addString: aString as: aFileName [
	| newMember |
	newMember := self memberClass newFromString: aString named: aFileName.
	self addMember: newMember.
	newMember localFileName: aFileName.
	^newMember
]

{ #category : 'archive operations' }
Archive >> addTree: aFileReference relativeTo: relativePath match: aBlock [
	| matches |
	matches := aFileReference  entries select: [ :entry | aBlock value: entry ].
	matches
		do: [ :entry | | newMember archiveFilePath |
			archiveFilePath := entry asFileReference relativeTo: relativePath.
			newMember := entry addArchiveMemberAs: archiveFilePath pathString in: self.
			entry isDirectory
				ifTrue: [ self addTree: entry asFileReference relativeTo: relativePath match: aBlock ].
	]
]

{ #category : 'archive operations' }
Archive >> canWriteToFile: aFileReferenceOrFileName [
	"Catch attempts to overwrite existing zip file"
	^(members anySatisfy: [ :each | each usesFile: aFileReferenceOrFileName ]) not
]

{ #category : 'archive operations' }
Archive >> contentsOf: aMemberOrName [
	| member |
	member := self member: aMemberOrName.
	member ifNil: [ ^nil ].
	^member contents
]

{ #category : 'archive operations' }
Archive >> extractMember: aMemberOrName [
	"Extract the member under the working directory, at the path given by its name, replacing an existing file"

	| member |
	member := self member: aMemberOrName.
	member ifNil: [ ^ nil ].
	member extractInDirectory: FileSystem workingDirectory overwrite: true
]

{ #category : 'archive operations' }
Archive >> extractMember: aMemberOrName toFileNamed: aFileName [
	| member |
	member := self member: aMemberOrName.
	member ifNil: [ ^nil ].
	member extractToFileNamed: aFileName
]

{ #category : 'archive operations' }
Archive >> extractMemberWithoutPath: aMemberOrName [
	self
		extractMemberWithoutPath: aMemberOrName
		inDirectory: FileSystem workingDirectory
]

{ #category : 'archive operations' }
Archive >> extractMemberWithoutPath: aMemberOrName inDirectory: dir [
	"Extract the member directly into dir, under the last part of its name"

	| member file |
	member := self member: aMemberOrName.
	member ifNil: [ ^ nil ].
	file := dir / member splitFileName last.
	member ensureExtractionTarget: file isInside: dir.
	member extractToFile: file overwrite: true
]

{ #category : 'initialization' }
Archive >> initialize [
	super initialize.
	members := OrderedCollection new
]

{ #category : 'private' }
Archive >> member: aMemberOrName [
	^(members includes: aMemberOrName)
		ifTrue: [ aMemberOrName ]
		ifFalse: [ self memberNamed: aMemberOrName ]
]

{ #category : 'private' }
Archive >> memberClass [
	self subclassResponsibility
]

{ #category : 'archive operations' }
Archive >> memberNamed: aString [
	"Return the first member whose zip name or local file name matches aString, or nil"
	^members detect: [ :ea | ea fileName = aString or: [ ea localFileName = aString ]] ifNone: [ ]
]

{ #category : 'archive operations' }
Archive >> memberNames [
	^members collect: [ :ea | ea fileName ]
]

{ #category : 'archive operations' }
Archive >> members [
	^members
]

{ #category : 'archive operations' }
Archive >> membersMatching: aString [
	^members select: [ :ea | (aString match: ea fileName) or: [ aString match: ea localFileName ] ]
]

{ #category : 'archive operations' }
Archive >> numberOfMembers [
	^members size
]

{ #category : 'archive operations' }
Archive >> removeMember: aMemberOrName [
	| member |
	member := self member: aMemberOrName.
	member ifNotNil: [ members remove: member ].
	^member
]

{ #category : 'archive operations' }
Archive >> replaceMember: aMemberOrName with: newMember [
	| member |
	member := self member: aMemberOrName.
	member ifNotNil: [ members replaceAll: member with: newMember ].
	^member
]

{ #category : 'archive operations' }
Archive >> setContentsOf: aMemberOrName to: aString [

	| newMember oldMember |
	oldMember := (self member: aMemberOrName) ifNil: [ ^ ZipArchiveError signal: 'no member named ' , aMemberOrName printString ].
	newMember := self memberClass newFromString: aString named: oldMember fileName.
	newMember copyAttributesFrom: oldMember.
	self replaceMember: oldMember with: newMember
]

{ #category : 'archive operations' }
Archive >> writeTo: aStream [
	self subclassResponsibility
]

{ #category : 'archive operations' }
Archive >> writeToFile: aFileReferenceOrFileName [
	"Catch attempts to overwrite existing zip file"

	| file |
	(self canWriteToFile: aFileReferenceOrFileName) ifFalse: [
		^ ZipArchiveError signal: aFileReferenceOrFileName asString , ' is needed by one or more members in this archive' ].
	file := aFileReferenceOrFileName asFileReference.
	file ensureDelete.
	file binaryWriteStreamDo: [ :stream | self writeTo: stream ]
]
