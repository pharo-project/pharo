Class {
	#name : #LGitPushStatusForeachCallback,
	#superclass : #LGitCallback,
	#category : #'LibGit-Core-FFI-Callbacks'
}

{ #category : #accessing }
LGitPushStatusForeachCallback class >> fnSpec [
	^#(int (char *arg1, char *arg2, void *arg3))
]
