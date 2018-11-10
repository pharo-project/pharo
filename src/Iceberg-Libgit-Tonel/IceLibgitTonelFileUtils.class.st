"
Emulates other 'file utils' classes by reading from a git repository (its blob, not a working copy'). Allows for reading any tree (from any commit) in the repository.
"
Class {
	#name : #IceLibgitTonelFileUtils,
	#superclass : #Object,
	#classVars : [
		'Current'
	],
	#category : #'Iceberg-Libgit-Tonel'
}

{ #category : #utilities }
IceLibgitTonelFileUtils class >> deleteAll: aTreeBuilder [ 
    ^ aTreeBuilder deleteAll
]

{ #category : #utilities }
IceLibgitTonelFileUtils class >> directoryExists: aLGitTreeEntry [ 
	^ aLGitTreeEntry type = LGitObjectTypeEnum git_obj_tree
]

{ #category : #utilities }
IceLibgitTonelFileUtils class >> directoryFromEntry: treeEntry [ 
	^ treeEntry object
]

{ #category : #utilities }
IceLibgitTonelFileUtils class >> directoryFromPath: aString relativeTo: aCommitOrTree [
	^ aCommitOrTree 
		entryByPath: aString 
		ifAbsent: [ 
			IceMissingRepositoryEntry new name: aString; owner: aCommitOrTree; signal ]

]

{ #category : #utilities }
IceLibgitTonelFileUtils class >> directoryName: anEntry [
    ^ anEntry filename
]

{ #category : #utilities }
IceLibgitTonelFileUtils class >> fileName: anEntry [
    ^ anEntry filename
]

{ #category : #accessing }
IceLibgitTonelFileUtils class >> pathNameDelimiter [
	^ $/
]

{ #category : #utilities }
IceLibgitTonelFileUtils class >> readStreamFor: fileName in: tree do: aBlock [
	(tree entryByPath: fileName) readStreamDo: aBlock
]

{ #category : #utilities }
IceLibgitTonelFileUtils class >> writeStreamFor: fileName in: directory do: aBlock [ 
	directory addEntryNamed: fileName withContents: (String streamContents: aBlock).
]
