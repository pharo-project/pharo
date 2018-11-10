Class {
	#name : #LGitWriteBackValueSizeT,
	#superclass : #LGitWriteBackValue,
	#category : #'LibGit-Core-FFI-Handling'
}

{ #category : #accessing }
LGitWriteBackValueSizeT class >> typeDecl [
	^ 'size_t'
]
