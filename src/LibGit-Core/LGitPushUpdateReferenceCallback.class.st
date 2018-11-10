Class {
	#name : #LGitPushUpdateReferenceCallback,
	#superclass : #LGitCallback,
	#category : #'LibGit-Core-FFI-Callbacks'
}

{ #category : #'callback spec' }
LGitPushUpdateReferenceCallback class >> fnSpec [
	^ #(int (String refname, String status, void *data))
]
