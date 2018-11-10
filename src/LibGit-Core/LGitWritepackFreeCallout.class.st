Class {
	#name : #LGitWritepackFreeCallout,
	#superclass : #LGitCallout,
	#category : #'LibGit-Core-FFI-Callouts'
}

{ #category : #'libgit-calls' }
LGitWritepackFreeCallout >> writepack: writepack [
	^ self call: #(void #(LGitOdbWritepack *writepack)) options: #()
]
