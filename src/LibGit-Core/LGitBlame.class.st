Class {
	#name : #LGitBlame,
	#superclass : #LGitRepositoryObject,
	#category : #'LibGit-Core-NonCoreObjects'
}

{ #category : #'libgit-calls' }
LGitBlame class >> blame_free: handle [
	^ self call: #(void git_blame_free #(void *handle)) options: #()
]

{ #category : #'libgit-finalization' }
LGitBlame class >> freeFunctionSelector [
	^ #blame_free:
]

{ #category : #'instance creation' }
LGitBlame class >> of: aRepository on: aFileReference [
	^ (self of: aRepository)
		getForReference: aFileReference;
		yourself
]

{ #category : #'libgit-calls' }
LGitBlame >> blame_buffer: out reference: reference buffer: buffer buffer_len: buffer_len [
	
	^ self
		callUnchecked:
			#(FFIInt32 git_blame_buffer #(LGitBlame *self , LGitBlame reference , String buffer , size_t buffer_len))
		options: #()
]

{ #category : #'libgit-calls' }
LGitBlame >> blame_file: out repo: repo path: path options: options [
	
	^ self
		callUnchecked:
			#(int git_blame_file #(LGitBlame * self , LGitRepository repo , String path , LGitBlameOptions * options))
		options: #()
]

{ #category : #'libgit-calls' }
LGitBlame >> blame_get_hunk_byindex: blame index: index [
	"todo: using LGitBlameHunk* as return value causes random crashes"
	
	^ self
		call: #(void * git_blame_get_hunk_byindex #(self , uint32 index))
		options: #()
]

{ #category : #'libgit-calls' }
LGitBlame >> blame_get_hunk_byline: blame lineno: lineno [
	"todo: using LGitBlameHunk* as return value causes random crashes"
	
	^ self
		call: #(void * git_blame_get_hunk_byline #(self , size_t lineno))
		options: #()
]

{ #category : #'libgit-calls' }
LGitBlame >> blame_get_hunk_count: blame [
	
	^ self
		call: #(uint32 git_blame_get_hunk_count #(self))
		options: #()
]

{ #category : #operations }
LGitBlame >> getForReference: aFileReference [
	^ self withReturnHandlerDo: [
		self
			blame_file: self
			repo: self repository
			path: ((aFileReference relativeTo: self repository repositoryReference) printWithDelimiter: $/)
			options: LGitBlameOptions defaults ]
]

{ #category : #operations }
LGitBlame >> hunkAt: anInteger [
	"use 1 based offset"
	^ LGitBlameHunk fromExternalAddress: (
		self
			blame_get_hunk_byindex: self
			index: anInteger - 1)
]

{ #category : #operations }
LGitBlame >> hunkAtLineNumber: anInteger [
	"first line has index 1"
	^ LGitBlameHunk fromExternalAddress: (
		self
			blame_get_hunk_byline: self
			lineno: anInteger)
]

{ #category : #operations }
LGitBlame >> numberOfHunks [
	^ self blame_get_hunk_count: self
]
