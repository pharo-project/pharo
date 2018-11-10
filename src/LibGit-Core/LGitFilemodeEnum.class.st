Class {
	#name : #LGitFilemodeEnum,
	#superclass : #LGitExternalEnumerationUInt32,
	#category : #'LibGit-Core-FFI-Enums'
}

{ #category : #'enum declaration' }
LGitFilemodeEnum class >> enumDecl [
	^#(
	GIT_FILEMODE_NEW 0
	GIT_FILEMODE_TREE 16384
	GIT_FILEMODE_BLOB 33188
	GIT_FILEMODE_BLOB_EXECUTABLE 33261
	GIT_FILEMODE_LINK 40960
	GIT_FILEMODE_COMMIT 57344
)
]

{ #category : #'accessing-values' }
LGitFilemodeEnum class >> git_filemode_blob [
	^ self basicNew
		value: 33188;
		yourself
]

{ #category : #'accessing-values' }
LGitFilemodeEnum class >> git_filemode_blob_executable [
	^ self basicNew
		value: 33261;
		yourself
]

{ #category : #'accessing-values' }
LGitFilemodeEnum class >> git_filemode_commit [
	^ self basicNew
		value: 57344;
		yourself
]

{ #category : #'accessing-values' }
LGitFilemodeEnum class >> git_filemode_link [
	^ self basicNew
		value: 40960;
		yourself
]

{ #category : #'accessing-values' }
LGitFilemodeEnum class >> git_filemode_new [
	^ self basicNew
		value: 0;
		yourself
]

{ #category : #'accessing-values' }
LGitFilemodeEnum class >> git_filemode_tree [
	^ self basicNew
		value: 16384;
		yourself
]
