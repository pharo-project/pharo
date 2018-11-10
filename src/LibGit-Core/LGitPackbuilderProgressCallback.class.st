Class {
	#name : #LGitPackbuilderProgressCallback,
	#superclass : #LGitCallback,
	#category : #'LibGit-Core-FFI-Callbacks'
}

{ #category : #'callback spec' }
LGitPackbuilderProgressCallback class >> fnSpec [
	^ #(int (int stage, uint32 current, uint32 total, void *payload))
]
