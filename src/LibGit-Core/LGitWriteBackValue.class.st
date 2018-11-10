Class {
	#name : #LGitWriteBackValue,
	#superclass : #FFIExternalValueHolder,
	#category : #'LibGit-Core-FFI-Handling'
}

{ #category : #testing }
LGitWriteBackValue class >> isAbstract [
	^ self == LGitWriteBackValue
]
