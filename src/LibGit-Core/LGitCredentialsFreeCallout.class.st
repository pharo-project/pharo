Class {
	#name : #LGitCredentialsFreeCallout,
	#superclass : #LGitCallout,
	#category : #'LibGit-Core-FFI-Callouts'
}

{ #category : #'libgit-calls' }
LGitCredentialsFreeCallout >> credentials: credentials [
	^ self
		call: #(void #(LGitCredentials * credentials))
		options: #(optMayGC)
]
