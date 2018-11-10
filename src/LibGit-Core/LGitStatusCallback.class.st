Class {
	#name : #LGitStatusCallback,
	#superclass : #LGitCallback,
	#category : #'LibGit-Core-FFI-Callbacks'
}

{ #category : #accessing }
LGitStatusCallback class >> fnSpec [
	^#(int (String path, uint status_flags, void *payload))
]
