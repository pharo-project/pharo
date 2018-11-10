Class {
	#name : #LGitStructWithDefaults,
	#superclass : #LGitExternalStructure,
	#category : #'LibGit-Core-FFI-Structs'
}

{ #category : #'instance creation' }
LGitStructWithDefaults class >> defaults [
	^ self externalNew
		initializeWithDefaults;
		yourself
]

{ #category : #initialization }
LGitStructWithDefaults >> initialize [
	super initialize.
	
	"Options are only ever used during a single setup function call.
	The contents of the struct are then copied to a libgit2 internal
	struct."
	self autoRelease
]

{ #category : #operations }
LGitStructWithDefaults >> initializeWithDefaults [
	self subclassResponsibility
]
