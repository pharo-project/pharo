Class {
	#name : #LGitBufferSignatureCallback,
	#superclass : #LGitCallback,
	#category : #'LibGit-Core-FFI-Callbacks'
}

{ #category : #'callback spec' }
LGitBufferSignatureCallback class >> fnSpec [
	^ #(int (void **out, LGitDiffFile *file, char *buf, size_t buflen, void *payload))
]
