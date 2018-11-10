"
I represent an external array of commits.
"
Class {
	#name : #LGitExternalCommitArray,
	#superclass : #LGitExternalArray,
	#category : #'LibGit-Core-HelperObjects'
}

{ #category : #'instance creation' }
LGitExternalCommitArray class >> fromCollection: aCollection [
	| instance |
	instance := self externalNew: aCollection size.
	aCollection withIndexDo: [ :commit :index |
		instance at: index put: commit ].
	
	^ instance
]

{ #category : #private }
LGitExternalCommitArray class >> type [ 
	^ LGitCommit
]

{ #category : #accessing }
LGitExternalCommitArray >> at: anIndex [
	^ LGitCommit fromHandle: (super at: anIndex)
]

{ #category : #accessing }
LGitExternalCommitArray >> at: anIndex put: aLGitCommit [
	^ super at: anIndex put: aLGitCommit getHandle
]
