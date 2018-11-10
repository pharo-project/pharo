Class {
	#name : #LGitCommitStore,
	#superclass : #FileSystemStore,
	#instVars : [
		'commit'
	],
	#category : #'LibGit-FileSystem'
}

{ #category : #'instance creation' }
LGitCommitStore class >> on: aLGitCommit [ 
	
	^ self new
		commit: aLGitCommit;
		yourself
]

{ #category : #public }
LGitCommitStore >> basenameFromEntry: aLGitTreeEntry [ 
	
	^ aLGitTreeEntry filename
]

{ #category : #abstract }
LGitCommitStore >> basicCreationTimeOf: aLGitTreeEntry [ 
	
	^ commit time asDateAndTime
]

{ #category : #abstract }
LGitCommitStore >> basicEntry: directoryEntry path: aPath nodesDo: aBlock [
	
	directoryEntry entries do: [ :each | aBlock value: each ]
]

{ #category : #abstract }
LGitCommitStore >> basicIsDirectory: aNode [
	
	^ aNode object isTree
]

{ #category : #abstract }
LGitCommitStore >> basicIsFile: aLGitTreeEntry [ 
	
	^ aLGitTreeEntry object isBlob
]

{ #category : #'error signalling' }
LGitCommitStore >> basicOpen: path writable: aBoolean [ 
	
	^ self
		nodeAt: path
		ifPresent: [ :aMemoryFileSystemEntry | aMemoryFileSystemEntry ]
		ifAbsent: [ 
			aBoolean
				ifFalse: [ self signalFileDoesNotExist: path ]
				ifTrue: [ self createFile: path ] ]
]

{ #category : #abstract }
LGitCommitStore >> basicSizeOf: aLGitTreeEntry [ 
	
	^ (self basicIsDirectory: aLGitTreeEntry)
		ifTrue: [ 0 ]
		ifFalse: [ aLGitTreeEntry object rawSize ]
]

{ #category : #accessing }
LGitCommitStore >> commit: anIceGitCommit [ 
	commit := anIceGitCommit
]

{ #category : #accessing }
LGitCommitStore >> delimiter [
	
	^ $/
]

{ #category : #printing }
LGitCommitStore >> forReferencePrintOn: aStream [
	
	aStream nextPutAll: 'git://'
]

{ #category : #accessing }
LGitCommitStore >> handleClass [
	
	^ LGitFileHandle
]

{ #category : #printing }
LGitCommitStore >> humanReadableSIByteSize [
	
	^ 0
]

{ #category : #printing }
LGitCommitStore >> nodeAt: anAbsolutePath ifPresent: aBlockClosure ifAbsent: aBlockClosure3 [ 
	
	anAbsolutePath isRoot ifTrue: [ ^ aBlockClosure value: commit tree ].	
	^ aBlockClosure value: (commit tree
		entryByPath: anAbsolutePath pathString
		ifAbsent: [ ^ aBlockClosure3 value ])
]
