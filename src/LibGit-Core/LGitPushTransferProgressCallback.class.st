Class {
	#name : #LGitPushTransferProgressCallback,
	#superclass : #LGitCallback,
	#category : #'LibGit-Core-FFI-Callbacks'
}

{ #category : #'callback spec' }
LGitPushTransferProgressCallback class >> fnSpec [
	^ #(int (uint current, uint total, size_t bytes, void *payload))
]
