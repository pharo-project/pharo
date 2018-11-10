Class {
	#name : #LGitErrorTypeEnum,
	#superclass : #LGitExternalEnumerationInt32,
	#category : #'LibGit-Core-FFI-Enums'
}

{ #category : #'enum declaration' }
LGitErrorTypeEnum class >> enumDecl [
	^ #(
		GITERR_NONE 0
		GITERR_NOMEMORY		1
		GITERR_OS				2
		GITERR_INVALID		3	
		GITERR_REFERENCE		4
		GITERR_ZLIB			5
		GITERR_REPOSITORY	6
		GITERR_CONFIG	7
		GITERR_REGEX	8
		GITERR_ODB	9
		GITERR_INDEX			10
		GITERR_OBJECT			11
		GITERR_NET				12
		GITERR_TAG			13
		GITERR_TREE			14
		GITERR_INDEXER		15
		GITERR_SSL				16
		GITERR_SUBMODULE	17
		GITERR_THREAD		18
		GITERR_STASH			19
		GITERR_CHECKOUT		20
		GITERR_FETCHHEAD	21
		GITERR_MERGE			22
		GITERR_SSH	23
		GITERR_FILTER 24
		GITERR_REVERT	25
		GITERR_CALLBACK	26
		GITERR_CHERRYPICK	27
		GITERR_DESCRIBE	28
		GITERR_REBASE	29
		GITERR_FILESYSTEM	30)
]

{ #category : #'accessing-values' }
LGitErrorTypeEnum class >> giterr_callback [
	^ self basicNew
		value: 26;
		yourself
]

{ #category : #'accessing-values' }
LGitErrorTypeEnum class >> giterr_checkout [
	^ self basicNew
		value: 20;
		yourself
]

{ #category : #'accessing-values' }
LGitErrorTypeEnum class >> giterr_cherrypick [
	^ self basicNew
		value: 27;
		yourself
]

{ #category : #'accessing-values' }
LGitErrorTypeEnum class >> giterr_config [
	^ self basicNew
		value: 7;
		yourself
]

{ #category : #'accessing-values' }
LGitErrorTypeEnum class >> giterr_describe [
	^ self basicNew
		value: 28;
		yourself
]

{ #category : #'accessing-values' }
LGitErrorTypeEnum class >> giterr_fetchhead [
	^ self basicNew
		value: 21;
		yourself
]

{ #category : #'accessing-values' }
LGitErrorTypeEnum class >> giterr_filesystem [
	^ self basicNew
		value: 30;
		yourself
]

{ #category : #'accessing-values' }
LGitErrorTypeEnum class >> giterr_filter [
	^ self basicNew
		value: 24;
		yourself
]

{ #category : #'accessing-values' }
LGitErrorTypeEnum class >> giterr_index [
	^ self basicNew
		value: 10;
		yourself
]

{ #category : #'accessing-values' }
LGitErrorTypeEnum class >> giterr_indexer [
	^ self basicNew
		value: 15;
		yourself
]

{ #category : #'accessing-values' }
LGitErrorTypeEnum class >> giterr_invalid [
	^ self basicNew
		value: 3;
		yourself
]

{ #category : #'accessing-values' }
LGitErrorTypeEnum class >> giterr_merge [
	^ self basicNew
		value: 22;
		yourself
]

{ #category : #'accessing-values' }
LGitErrorTypeEnum class >> giterr_net [
	^ self basicNew
		value: 12;
		yourself
]

{ #category : #'accessing-values' }
LGitErrorTypeEnum class >> giterr_nomemory [
	^ self basicNew
		value: 1;
		yourself
]

{ #category : #'accessing-values' }
LGitErrorTypeEnum class >> giterr_none [
	^ self basicNew
		value: 0;
		yourself
]

{ #category : #'accessing-values' }
LGitErrorTypeEnum class >> giterr_object [
	^ self basicNew
		value: 11;
		yourself
]

{ #category : #'accessing-values' }
LGitErrorTypeEnum class >> giterr_odb [
	^ self basicNew
		value: 9;
		yourself
]

{ #category : #'accessing-values' }
LGitErrorTypeEnum class >> giterr_os [
	^ self basicNew
		value: 2;
		yourself
]

{ #category : #'accessing-values' }
LGitErrorTypeEnum class >> giterr_rebase [
	^ self basicNew
		value: 29;
		yourself
]

{ #category : #'accessing-values' }
LGitErrorTypeEnum class >> giterr_reference [
	^ self basicNew
		value: 4;
		yourself
]

{ #category : #'accessing-values' }
LGitErrorTypeEnum class >> giterr_regex [
	^ self basicNew
		value: 8;
		yourself
]

{ #category : #'accessing-values' }
LGitErrorTypeEnum class >> giterr_repository [
	^ self basicNew
		value: 6;
		yourself
]

{ #category : #'accessing-values' }
LGitErrorTypeEnum class >> giterr_revert [
	^ self basicNew
		value: 25;
		yourself
]

{ #category : #'accessing-values' }
LGitErrorTypeEnum class >> giterr_ssh [
	^ self basicNew
		value: 23;
		yourself
]

{ #category : #'accessing-values' }
LGitErrorTypeEnum class >> giterr_ssl [
	^ self basicNew
		value: 16;
		yourself
]

{ #category : #'accessing-values' }
LGitErrorTypeEnum class >> giterr_stash [
	^ self basicNew
		value: 19;
		yourself
]

{ #category : #'accessing-values' }
LGitErrorTypeEnum class >> giterr_submodule [
	^ self basicNew
		value: 17;
		yourself
]

{ #category : #'accessing-values' }
LGitErrorTypeEnum class >> giterr_tag [
	^ self basicNew
		value: 13;
		yourself
]

{ #category : #'accessing-values' }
LGitErrorTypeEnum class >> giterr_thread [
	^ self basicNew
		value: 18;
		yourself
]

{ #category : #'accessing-values' }
LGitErrorTypeEnum class >> giterr_tree [
	^ self basicNew
		value: 14;
		yourself
]

{ #category : #'accessing-values' }
LGitErrorTypeEnum class >> giterr_zlib [
	^ self basicNew
		value: 5;
		yourself
]
