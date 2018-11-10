Class {
	#name : #LGitTransferProgressCallback,
	#superclass : #LGitCallback,
	#category : #'LibGit-Core-FFI-Callbacks'
}

{ #category : #'callback spec' }
LGitTransferProgressCallback class >> fnSpec [
	^#(int (LGitTransferProgress *stats, void *payload))
]
