Class {
	#name : #LGitRemoteCreateCallback,
	#superclass : #LGitCallback,
	#category : #'LibGit-Core-FFI-Callbacks'
}

{ #category : #'callback spec' }
LGitRemoteCreateCallback class >> fnSpec [
	^ #(int (void **out, LGitRepository repo, String name, String url, void *payload))
]
