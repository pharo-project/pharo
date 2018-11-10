Class {
	#name : #LGitCheckoutProgressCallback,
	#superclass : #LGitCallback,
	#category : #'LibGit-Core-FFI-Callbacks'
}

{ #category : #accessing }
LGitCheckoutProgressCallback class >> fnSpec [
	^#(void (String* path, size_t completed_steps, size_t total_steps, void* payload))
]
