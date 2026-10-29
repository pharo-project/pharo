"
I'm a specific store for Zip file systems
"
Class {
	#name : 'ZipStore',
	#superclass : 'MemoryStore',
	#instVars : [
		'reference'
	],
	#category : 'FileSystem-Zip-Base',
	#package : 'FileSystem-Zip',
	#tag : 'Base'
}

{ #category : 'instance creation' }
ZipStore class >> reference: aReference [
	^ self basicNew
		initializeWithReference: aReference
		yourself
]

{ #category : 'public' }
ZipStore >> close [

	| archive fs |
	archive := ZipArchive new.
	fs := FileSystem store: self.
	fs root allChildren do: [ :each |
			| name |
			name := (each relativeTo: fs root) pathString.
			each isFile ifTrue: [ each binaryReadStreamDo: [ :input | archive addDeflateString: input upToEnd as: name ] ].
			(each isDirectory and: [ each ~= fs root ]) ifTrue: [ archive addDirectory: each as: name ] ].
	self reference ensureDelete.
	self reference binaryWriteStreamDo: [ :output | archive writeTo: output ].
	archive close
]

{ #category : 'printing' }
ZipStore >> forReferencePrintOn: aStream [
	aStream nextPutAll: 'zip://'
]

{ #category : 'initialization' }
ZipStore >> initialize [
	super initialize.
	root := ZipFileSystemDirectory new
]

{ #category : 'initialization' }
ZipStore >> initializeWithReference: aReference [
	self initialize.
	reference := aReference
]

{ #category : 'public' }
ZipStore >> open [
	| archive |
	self reference exists
		ifFalse: [ ^ self ].
	archive := ZipArchive new.
	self reference
		binaryReadStreamDo: [ :input |
			archive readFrom: input contents readStream.
			archive members
				do: [ :member |
					| path |
					path := self pathFromMember: member.
					member isDirectory
						ifTrue: [ self ensureCreateDirectory: path ]
						ifFalse: [
							self ensureCreateDirectory: path parent.
							(self createFile: path) member: member ] ] ].
	^ self
]

{ #category : 'private' }
ZipStore >> pathFromMember: anArchiveMember [
	| path |
	path := Path from: anArchiveMember fileName.
	^ path basename isEmpty
		ifTrue: [ path parent ]
		ifFalse: [ path ]
]

{ #category : 'public' }
ZipStore >> reference [
	^ reference
]
