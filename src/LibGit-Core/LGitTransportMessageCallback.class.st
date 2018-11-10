Class {
	#name : #LGitTransportMessageCallback,
	#superclass : #LGitCallback,
	#category : #'LibGit-Core-FFI-Callbacks'
}

{ #category : #'callback spec' }
LGitTransportMessageCallback class >> fnSpec [
	^#(int (String str, int len, void *payload))
]
