Class {
	#name : #LGitCompletionCallback,
	#superclass : #LGitCallback,
	#category : #'LibGit-Core-FFI-Callbacks'
}

{ #category : #'callback spec' }
LGitCompletionCallback class >> fnSpec [
	^#(int (LGitRemoteCompletionTypeEnum type, void *argument))
]
