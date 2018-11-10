Class {
	#name : #LGitRepositoryCreateCallback,
	#superclass : #LGitCallback,
	#category : #'LibGit-Core-FFI-Callbacks'
}

{ #category : #'callback spec' }
LGitRepositoryCreateCallback class >> fnSpec [
	^ #(int (LGitRepository **out, String path, Boolean bare, void *payload))
]
