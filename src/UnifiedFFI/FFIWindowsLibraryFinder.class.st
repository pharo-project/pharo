"
I find libraries in a Windows system.
"
Class {
	#name : 'FFIWindowsLibraryFinder',
	#superclass : 'FFILibraryFinder',
	#category : 'UnifiedFFI-Libraries',
	#package : 'UnifiedFFI',
	#tag : 'Libraries'
}

{ #category : 'accessing' }
FFIWindowsLibraryFinder >> basePaths [

	^ {
	Smalltalk imageDirectory fullName.
	Smalltalk vm directory }
]

{ #category : 'accessing' }
FFIWindowsLibraryFinder >> knownPaths [

	^ (OSPlatform current environmentVariableNamed: 'PATH' ifAbsent: [ '' ]) substrings: ';'
]
