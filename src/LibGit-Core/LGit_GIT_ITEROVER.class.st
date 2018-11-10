Class {
	#name : #'LGit_GIT_ITEROVER',
	#superclass : #LGitCallReturnHandler,
	#category : #'LibGit-Core-Exceptions-ReturnCodes'
}

{ #category : #signaling }
LGit_GIT_ITEROVER >> signal [
	"don't signal, everything is fine"
	^ self
]
