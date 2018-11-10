Class {
	#name : #LGitCheckoutPerfDataCallback,
	#superclass : #LGitCallback,
	#category : #'LibGit-Core-FFI-Callbacks'
}

{ #category : #accessing }
LGitCheckoutPerfDataCallback class >> fnSpec [
	^#(void (LGitCheckoutPerfData *perfdata, void *payload))
]
