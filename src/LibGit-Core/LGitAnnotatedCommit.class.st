Class {
	#name : #LGitAnnotatedCommit,
	#superclass : #LGitRepositoryObject,
	#category : #'LibGit-Core-NonCoreObjects'
}

{ #category : #'libgit-calls' }
LGitAnnotatedCommit class >> annotated_commit_free: handle [
	^ self
		call: #(void git_annotated_commit_free #(void *handle))
		options: #()
]

{ #category : #'ligbit-finalization' }
LGitAnnotatedCommit class >> freeFunctionSelector [
	^ #annotated_commit_free:
]

{ #category : #'instance creation' }
LGitAnnotatedCommit class >> fromRef: anLGitReference [
	^ (self of: anLGitReference repository) initializeFromRef: anLGitReference 
]

{ #category : #initialize }
LGitAnnotatedCommit >> annotated_commit_from_ref: ref [
	^ self callUnchecked: #(
		int git_annotated_commit_from_ref #(
			LGitAnnotatedCommit * self , 
			LGitRepository repository ,
			LGitReference ref)) 
		options: #()
]

{ #category : #'libgit-calls' }
LGitAnnotatedCommit >> annotated_commit_id [
	 ^ self call: #(LGitId * git_annotated_commit_id #(self)) options: #()
]

{ #category : #accessing }
LGitAnnotatedCommit >> id [
	^ self annotated_commit_id
]

{ #category : #initialize }
LGitAnnotatedCommit >> initializeFromRef: anLGitReference [
	self withReturnHandlerDo: [ 
		self annotated_commit_from_ref: anLGitReference
	].

	self autoRelease
]
