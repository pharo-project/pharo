Class {
	#name : #LGitFileSignatureCallback,
	#superclass : #LGitCallback,
	#category : #'LibGit-Core-FFI-Callbacks'
}

{ #category : #'callback spec' }
LGitFileSignatureCallback class >> fnSpec [
	^ #(int (void **out, LGitDiffFile *file, String fullpath, void *payload))
]
