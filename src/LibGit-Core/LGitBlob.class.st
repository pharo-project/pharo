Class {
	#name : #LGitBlob,
	#superclass : #LGitObject,
	#category : #'LibGit-Core-CoreObjects'
}

{ #category : #'libgit-calls' }
LGitBlob class >> blob_create_frombuffer: objectId repo: repo buffer: buffer len: len [
	
	^ self
		call:
			#(LGitReturnCodeEnum git_blob_create_frombuffer #(LGitId * objectId , LGitRepository repo , void  *buffer , size_t len))
		options: #()
]

{ #category : #'libgit-calls' }
LGitBlob class >> blob_create_fromdisk: objectId repo: repo path: path [
	
	^ self
		call:
			#(LGitReturnCodeEnum git_blob_create_fromdisk #(LGitId * objectId , LGitRepository repo , FFIExternalString path))
		options: #()
]

{ #category : #'libgit-calls' }
LGitBlob class >> blob_free: handle [
	
	^ self
		call: 	#(void git_blob_free(void *handle))
		options: #()
]

{ #category : #'accessing-defaults' }
LGitBlob class >> defaultFilemode [
	^ LGitFilemodeEnum git_filemode_blob
]

{ #category : #'libgit-finalization' }
LGitBlob class >> freeFunctionSelector [
	^ #blob_free:
]

{ #category : #testing }
LGitBlob class >> isType: aType [
	^ aType = LGitObjectTypeEnum git_obj_blob
]

{ #category : #'instance creation' }
LGitBlob class >> of: aRepository fromExternalCollection: aCollection [
	| id |
	self assert: (aCollection class = ByteArray or: [ aCollection class classSide canUnderstand: #asExternalTypeOn: ]).
	id := LGitId new.
	self
		blob_create_frombuffer: id
		repo: aRepository
		buffer: aCollection
		len: aCollection size.
	^ (self of: aRepository)
		initializeWithId: id;
		yourself
]

{ #category : #'instance creation' }
LGitBlob class >> of: aRepository fromFile: aFileReference [
	| id |
	id := LGitId new.
	self blob_create_fromdisk: id repo: aRepository path: aFileReference fullName.
	^ (self of: aRepository)
		initializeWithId: id;
		yourself
]

{ #category : #'instance creation' }
LGitBlob class >> of: aRepository fromString: aString [
	^ self of: aRepository fromExternalCollection: aString
]

{ #category : #comparing }
LGitBlob >> = anObject [
	^ self == anObject or: [
		self class == anObject class and: [
			self isReady and: [
				anObject isReady and: [
					self id = anObject id ] ] ] ]
]

{ #category : #'private-accessing' }
LGitBlob >> binaryContents [
	| bytes |
	"freeing not required. See #rawBuffer"
	bytes := ByteArray new: self rawSize.
	LibC memCopy: self rawBuffer to: bytes size: bytes size.
	^bytes
]

{ #category : #'libgit-calls' }
LGitBlob >> blob_id: blob [
	
	^ self call: #(void * git_blob_id #(self)) options: #()
]

{ #category : #'libgit-calls' }
LGitBlob >> blob_is_binary: blob [
	
	^ self call: #(Boolean git_blob_is_binary #(self)) options: #()
]

{ #category : #'libgit-calls' }
LGitBlob >> blob_lookup: blob repo: repo id: objectId [
	
	^ self
		callUnchecked:
			#(LGitReturnCodeEnum git_blob_lookup #(LGitBlob * blob , LGitRepository repo , LGitId * objectId))
		options: #()
]

{ #category : #'libgit-calls' }
LGitBlob >> blob_rawcontent: blob [
	
	^ self call: #(void * git_blob_rawcontent #(self)) options: #()
]

{ #category : #'libgit-calls' }
LGitBlob >> blob_rawsize: blob [
	
	^ self call: #(int64 git_blob_rawsize #(self)) options: #()
]

{ #category : #accessing }
LGitBlob >> contents [
	^ self isBinary
		ifTrue:  [ self binaryContents ]
		ifFalse: [ self rawBuffer readStringUTF8 ]
	
	
]

{ #category : #private }
LGitBlob >> get_id_data [
	^ self blob_id: self
]

{ #category : #comparing }
LGitBlob >> hash [
	^ self contents hash
]

{ #category : #testing }
LGitBlob >> isBinary [
	^ self blob_is_binary: self
]

{ #category : #testing }
LGitBlob >> isBlob [
	^ true
]

{ #category : #private }
LGitBlob >> lookup [
	self withReturnHandlerDo: [ self
		blob_lookup: self 
		repo: self repository
		id: self id ].
	self autoRelease
]

{ #category : #accessing }
LGitBlob >> names [
	| names |
	names := OrderedCollection new.
	self repository head object tree allEntriesDo: [ :rootPath :entry :payload |
		entry repository: self repository.
		entry object = self ifTrue: [ names add: entry filename ].
		0 ].
	
	^ names
]

{ #category : #printing }
LGitBlob >> objectPrintOn: aStream [
	self id objectPrintOn: aStream
]

{ #category : #'private-accessing' }
LGitBlob >> rawBuffer [
	"freeing not required (see http://libgit2.github.com/libgit2/#HEAD/group/blob/git_blob_rawcontent)"
	^ self blob_rawcontent: self
]

{ #category : #'private-accessing' }
LGitBlob >> rawSize [
	^ self blob_rawsize: self
]
