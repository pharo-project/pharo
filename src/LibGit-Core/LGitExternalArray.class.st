"
I provide general implementations for my subclasses that represent dedicated arrays of a certain type.
"
Class {
	#name : #LGitExternalArray,
	#superclass : #FFIExternalArray,
	#category : #'LibGit-Core-HelperObjects'
}

{ #category : #'instance creation' }
LGitExternalArray class >> externalNew: numberOfElements [
	^ self 
		externalNewType: self type 
		size: numberOfElements
]

{ #category : #'instance creation' }
LGitExternalArray class >> onAddress: handle size: numberOfElements [
	^ self fromHandle: handle type: self type size: numberOfElements
]

{ #category : #private }
LGitExternalArray class >> type [ 
	^ self subclassResponsibility
]

{ #category : #converting }
LGitExternalArray >> asArray [
	| array |
	array := Array new: self size.
	1 to: self size do: [ :index |
		array
			at: index
			put: (self at: index) ].
		
	^ array
]
