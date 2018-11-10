Class {
	#name : #LGitWritepackCommitCallout,
	#superclass : #LGitCallout,
	#category : #'LibGit-Core-FFI-Callouts'
}

{ #category : #'libgit-calls' }
LGitWritepackCommitCallout >> writepack: writepack progress: progress [
	^ self
		call: #(int #(LGitOdbWritepack *writepack , LGitTransferProgress *progress))
		options: #(optMayGC)
]
