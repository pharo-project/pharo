"
Emulates other 'file utils' classes by reading from a git repository (its blob, not a working copy'). Allows for reading any tree (from any commit) in the repository.
"
Class {
	#name : #IceLibgitFiletreeFileUtils,
	#superclass : #Object,
	#classVars : [
		'Current'
	],
	#category : #'Iceberg-Libgit-Filetree'
}

{ #category : #utilities }
IceLibgitFiletreeFileUtils class >> deleteAll: aTreeBuilder [ 
    ^ aTreeBuilder deleteAll
]

{ #category : #utilities }
IceLibgitFiletreeFileUtils class >> directoryExists: aLGitTreeEntry [ 
	^ aLGitTreeEntry type = LGitObjectTypeEnum git_obj_tree
]

{ #category : #utilities }
IceLibgitFiletreeFileUtils class >> directoryFromEntry: treeEntry [ 
	^ treeEntry object
]

{ #category : #utilities }
IceLibgitFiletreeFileUtils class >> directoryFromPath: aString relativeTo: aCommitOrTree [
	^ aCommitOrTree 
		entryByPath: aString 
		ifAbsent: [ 
			IceMissingRepositoryEntry new name: aString; owner: aCommitOrTree; signal ]

]

{ #category : #utilities }
IceLibgitFiletreeFileUtils class >> directoryName: anEntry [
    ^ anEntry filename
]

{ #category : #utilities }
IceLibgitFiletreeFileUtils class >> fileName: anEntry [
    ^ anEntry filename
]

{ #category : #accessing }
IceLibgitFiletreeFileUtils class >> pathNameDelimiter [
	^ $/
]

{ #category : #utilities }
IceLibgitFiletreeFileUtils class >> readStreamFor: fileName in: tree do: aBlock [
	(tree entryByPath: fileName) readStreamDo: aBlock
]

{ #category : #utilities }
IceLibgitFiletreeFileUtils class >> writeStreamFor: fileName in: directory do: aBlock [ 
	directory addEntryNamed: fileName withContents: (String streamContents: aBlock).
]
