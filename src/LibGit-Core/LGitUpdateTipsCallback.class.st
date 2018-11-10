Class {
	#name : #LGitUpdateTipsCallback,
	#superclass : #LGitCallback,
	#category : #'LibGit-Core-FFI-Callbacks'
}

{ #category : #'callback spec' }
LGitUpdateTipsCallback class >> fnSpec [
	^ #(int (String refname, LGitId *a, LGitId *b, void * data))
]
