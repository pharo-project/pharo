Class {
	#name : #LGitFreeSignatureCallback,
	#superclass : #LGitCallback,
	#category : #'LibGit-Core-FFI-Callbacks'
}

{ #category : #'callback spec' }
LGitFreeSignatureCallback class >> fnSpec [
	^ #(void (void *sig, void *payload))
]
