"
My elements are 8 bit unsigned integers.
"
Class {
	#name : #LGitExternalUInt8Array,
	#superclass : #LGitExternalArray,
	#category : #'LibGit-Core-HelperObjects'
}

{ #category : #private }
LGitExternalUInt8Array class >> type [
	^ FFIUInt32
]

{ #category : #accessing }
LGitExternalUInt8Array >> type [ 
	^ FFIUInt8
]
