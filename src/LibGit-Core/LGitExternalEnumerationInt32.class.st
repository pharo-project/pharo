Class {
	#name : #LGitExternalEnumerationInt32,
	#superclass : #LGitExternalEnumerationUInt32,
	#category : #'LibGit-Core-Abstracts'
}

{ #category : #'accessing-defaults' }
LGitExternalEnumerationInt32 class >> defaultRepresentationType [
	^ FFIInt32 new
]
