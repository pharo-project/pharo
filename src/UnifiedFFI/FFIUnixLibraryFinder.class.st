"
I'm a base library finder for unix distributions. 
I'm abstract, you need to use one of my children.
"
Class {
	#name : #FFIUnixLibraryFinder,
	#superclass : #FFILibraryFinder,
	#category : #'UnifiedFFI-Libraries'
}

{ #category : #accessing }
FFIUnixLibraryFinder >> basePaths [
		
	^ { 
	Smalltalk imageDirectory fullName.
	Smalltalk vm directory }
]
