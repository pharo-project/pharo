Class {
	#name : #LGitObjectTypeEnum,
	#superclass : #LGitExternalEnumerationInt32,
	#category : #'LibGit-Core-FFI-Enums'
}

{ #category : #'enum declaration' }
LGitObjectTypeEnum class >> enumDecl [
	^#(
	GIT_OBJ_ANY -2
	GIT_OBJ_BAD -1
	GIT_OBJ__EXT1 0
	GIT_OBJ_COMMIT 1
	GIT_OBJ_TREE 2
	GIT_OBJ_BLOB 3
	GIT_OBJ_TAG 4
	GIT_OBJ__EXT2 5
	GIT_OBJ_OFS_DELTA 6
	GIT_OBJ_REF_DELTA 7
)
]

{ #category : #'accessing-values' }
LGitObjectTypeEnum class >> git_obj__ext1 [
	^ self basicNew
		value: 0;
		yourself
]

{ #category : #'accessing-values' }
LGitObjectTypeEnum class >> git_obj__ext2 [
	^ self basicNew
		value: 5;
		yourself
]

{ #category : #'accessing-values' }
LGitObjectTypeEnum class >> git_obj_any [
	^ self basicNew
		value: -2;
		yourself
]

{ #category : #'accessing-values' }
LGitObjectTypeEnum class >> git_obj_bad [
	^ self basicNew
		value: -1;
		yourself
]

{ #category : #'accessing-values' }
LGitObjectTypeEnum class >> git_obj_blob [
	^ self basicNew
		value: 3;
		yourself
]

{ #category : #'accessing-values' }
LGitObjectTypeEnum class >> git_obj_commit [
	^ self basicNew
		value: 1;
		yourself
]

{ #category : #'accessing-values' }
LGitObjectTypeEnum class >> git_obj_ofs_delta [
	^ self basicNew
		value: 6;
		yourself
]

{ #category : #'accessing-values' }
LGitObjectTypeEnum class >> git_obj_ref_delta [
	^ self basicNew
		value: 7;
		yourself
]

{ #category : #'accessing-values' }
LGitObjectTypeEnum class >> git_obj_tag [
	^ self basicNew
		value: 4;
		yourself
]

{ #category : #'accessing-values' }
LGitObjectTypeEnum class >> git_obj_tree [
	^ self basicNew
		value: 2;
		yourself
]
