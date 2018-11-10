Class {
	#name : #LGitPushNegotiationCallback,
	#superclass : #LGitCallback,
	#category : #'LibGit-Core-FFI-Callbacks'
}

{ #category : #'callback spec' }
LGitPushNegotiationCallback class >> fnSpec [
	^ #(int (LGitPushUpdate **updates, size_t len, void *payload))
]
