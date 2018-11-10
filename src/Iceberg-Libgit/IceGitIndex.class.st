Class {
	#name : #IceGitIndex,
	#superclass : #IceIndex,
	#instVars : [
		'modifiedFilePaths'
	],
	#category : #'Iceberg-Libgit-Core'
}

{ #category : #adding }
IceGitIndex >> addPackage: anIcePackageDefinition [ 
	
	| pathSegments |
	pathSegments := repository subdirectoryPath segments asOrderedCollection.
	pathSegments add: (repository writerClass directoryNameFor: anIcePackageDefinition).	
	self addPath: (RelativePath withAll: pathSegments)
]

{ #category : #storing }
IceGitIndex >> addPath: aPath [

	modifiedFilePaths add: aPath
]

{ #category : #operations }
IceGitIndex >> addToGitIndex [

	repository addFilesToIndex: modifiedFilePaths.
]

{ #category : #'API-commiting' }
IceGitIndex >> commitWithMessage: message andParents: parentCommits [
	
	| index |
	repository handleLibgitError: [ | signature commitId indexTreeId |
		self addToGitIndex.
		[ signature := self repositoryHandle defaultSignature ]
			on: LGit_GIT_ENOTFOUND, LGit_GIT_ERROR
			do: [ :e | IceGitUsernameOrEmailNotFound guessKind: e ifNot: [ e pass ] ].
		index := self repositoryHandle index.
		indexTreeId := index writeTreeAndReturnOid.
		index free.
		commitId := (LGitCommitBuilder of: self repositoryHandle)
			tree: (LGitTree of: self repositoryHandle fromId: indexTreeId);
			message: message withUnixLineEndings utf8Encoded asString "FFI is expecting an string";
			parents:
				(self repositoryHandle isUnborn
					ifTrue: [ #() ]
					ifFalse: [ self libGitCommitsFrom: self repositoryHandle for: parentCommits ]);
			"'HEAD' branch could not exist yet. No problem, libgit will create it, 
				but we have to remember it to notify the frontend."
				updateReference:
				([ self repositoryHandle head name ]
					on: LGit_GIT_EUNBORNBRANCH
					do: [ repository branch gitRef ]);
			author: signature;
			committer: signature;
			writeCommit;
			commitId.
		^ repository commitFromGitCommit: (LGitCommit of: self repositoryHandle fromId: commitId)]
]

{ #category : #accessing }
IceGitIndex >> includes: aRelativePath [ 
	
	^ modifiedFilePaths includes: aRelativePath
]

{ #category : #initialization }
IceGitIndex >> initialize [

	super initialize.
	modifiedFilePaths := Set new
]

{ #category : #testing }
IceGitIndex >> isEmpty [
	
	^ modifiedFilePaths isEmpty
]

{ #category : #'API-commiting' }
IceGitIndex >> libGitCommitsFrom: aLGitRepository for: aListOfCommitish [
	^ aListOfCommitish collect: 
		[ :commitish | 
		LGitCommit of: aLGitRepository fromId: (LGitId fromHexString: commitish id) ]
]

{ #category : #storing }
IceGitIndex >> removeFileDefinition: anIceFileDefinition [ 
	
	(repository location resolve: anIceFileDefinition path)
		ensureDelete.

	self addPath: anIceFileDefinition path
]

{ #category : #accessing }
IceGitIndex >> repositoryHandle [
	
	<repositoryHandleAccessor>
	^ repository repositoryHandle
]

{ #category : #storing }
IceGitIndex >> storeFileDefinition: anIceFileDefinition [ 
	
	(repository location resolve: anIceFileDefinition path)
		ensureDelete;
		binaryWriteStreamDo: [ :writeStream |
			writeStream nextPutAll: anIceFileDefinition binaryContents ].

	self addPath: anIceFileDefinition path
]

{ #category : #storing }
IceGitIndex >> storeMCVersion: aMCVersion [ 

	| pathSegments |
	repository internalStoreVersion: aMCVersion.
	pathSegments := OrderedCollection new.
	repository subdirectory
		ifNotEmpty: [ :directory | pathSegments add: directory ].
	pathSegments add: (repository writerClass directoryNameFor: aMCVersion package).
	
	self addPath: (RelativePath withAll: pathSegments)
]

{ #category : #storing }
IceGitIndex >> storeVersion: aMCVersion [ 
	
	repository internalStoreVersion: aMCVersion
]

{ #category : #updating }
IceGitIndex >> updateDiskWorkingCopy: anIceDiff [
	anIceDiff tree
		accept:
			(IceGitWorkingCopyUpdateVisitor new
				repository: repository;
				index: self;
				diff: anIceDiff)
]
