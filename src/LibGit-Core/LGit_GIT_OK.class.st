Class {
	#name : #'LGit_GIT_OK',
	#superclass : #LGitCallReturnHandler,
	#category : #'LibGit-Core-Exceptions-ReturnCodes'
}

{ #category : #testing }
LGit_GIT_OK class >> isError [
	^ false
]

{ #category : #signaling }
LGit_GIT_OK >> signal [
	"don't signal, everything is fine"
	^ self
]
