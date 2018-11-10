Class {
	#name : #LGitCallError,
	#superclass : #LGitAbstractError,
	#category : #'LibGit-Core-Exceptions'
}

{ #category : #testing }
LGitCallError class >> isAbstract [
	^ self == LGitCallError
]
