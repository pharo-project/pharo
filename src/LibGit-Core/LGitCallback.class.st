Class {
	#name : #LGitCallback,
	#superclass : #FFICallback,
	#category : #'LibGit-Core-FFI-Callbacks'
}

{ #category : #'instance creation' }
LGitCallback class >> null [
	^ self new initializeNull
]

{ #category : #'instance creation' }
LGitCallback class >> on: aBlock [
	^ self 
		signature: self fnSpec 
		block: aBlock 
]

{ #category : #'as yet unclassified' }
LGitCallback >> initializeNull [
	thunk := ExternalAddress null
]
