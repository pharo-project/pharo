Class {
	#name : #LGitTransportCallback,
	#superclass : #LGitCallback,
	#category : #'LibGit-Core-FFI-Callbacks'
}

{ #category : #'callback spec' }
LGitTransportCallback class >> fnSpec [
	^#(int git_transport_cb(LGitTransport *out, LGitRemote owner, void *param))
]
