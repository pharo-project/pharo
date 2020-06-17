"
I find libraries in a Windows system.
"
Class {
	#name : #FFIWindowsLibraryFinder,
	#superclass : #FFILibraryFinder,
	#category : #'UnifiedFFI-Libraries'
}

{ #category : #accessing }
FFIWindowsLibraryFinder >> basePaths [

	^ {
	Smalltalk imageDirectory fullName.
	Smalltalk vm directory }
]

{ #category : #accessing }
FFIWindowsLibraryFinder >> knownPaths [
		
	^ (OSEnvironment current at: 'PATH' ifAbsent: [ '' ]) substrings: ';'
]
