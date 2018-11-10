Class {
	#name : #LGitOdb,
	#superclass : #LGitExternalObject,
	#category : #'LibGit-Core-NonCoreObjects'
}

{ #category : #'libgit-finalization' }
LGitOdb class >> freeFunctionSelector [
	^ #odb_free:
]

{ #category : #'libgit-calls' }
LGitOdb class >> odb_free: handle [
	
	^ self call: #(void git_odb_free #(void *handle)) options: #()
]
