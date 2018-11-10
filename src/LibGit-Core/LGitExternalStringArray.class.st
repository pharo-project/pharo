"
I represent an external array of strings. Since strings are of variable size, they can not be easily represented by the general implementation.
I take care of converting regular strings to external ones (which allocates and reserves the memory) and reading strings from memory.

I override #free to ensure that the memory for the external strings is correctly freed.
"
Class {
	#name : #LGitExternalStringArray,
	#superclass : #LGitExternalArray,
	#category : #'LibGit-Core-HelperObjects'
}

{ #category : #'instance creation' }
LGitExternalStringArray class >> fromCollection: aCollection [
	| instance |
	instance := self externalNew: aCollection size.
	aCollection withIndexDo: [ :element :index |
		instance at: index put: element ].
	
	^ instance
]

{ #category : #private }
LGitExternalStringArray class >> type [
	^ 'char*'
]

{ #category : #accessing }
LGitExternalStringArray >> at: anInteger [
	^ (self primAt: anInteger) readString
]

{ #category : #accessing }
LGitExternalStringArray >> at: index put: aString [
	^ super at: index put: aString asLGitExternalString getHandle
]

{ #category : #'initialize-release' }
LGitExternalStringArray >> free [
	1 to: self size do: [ :index | 
		(self primAt: index) free ].
	
	super free
]

{ #category : #private }
LGitExternalStringArray >> primAt: anInteger [
	^ super at: anInteger
]
