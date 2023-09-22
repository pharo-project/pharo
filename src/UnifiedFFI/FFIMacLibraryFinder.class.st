"
I find libraries in a MacOS (64bit) system.
"
Class {
	#name : 'FFIMacLibraryFinder',
	#superclass : 'FFILibraryFinder',
	#category : 'UnifiedFFI-Libraries',
	#package : 'UnifiedFFI',
	#tag : 'Libraries'
}

{ #category : 'accessing' }
FFIMacLibraryFinder class >> findFramework: aName [

	^ self new findFramework: aName
]

{ #category : 'accessing' }
FFIMacLibraryFinder >> basePaths [

	^ {
	Smalltalk imageDirectory fullName.
	(Smalltalk vm binary parent / 'Plugins') fullName.
	(Smalltalk vm binary parent parent / 'Resources' / 'lib') fullName }
]

{ #category : 'accessing' }
FFIMacLibraryFinder >> findFramework: aName [
	| path |

	path :=  ('/System/Library/Frameworks/', aName, '.framework/', aName) asFileReference.
	path exists ifTrue: [ ^ path fullName ].

	self error: ('Framework {1} not found.' format: { aName })
]

{ #category : 'accessing' }
FFIMacLibraryFinder >> knownPaths [

	^ #(
	'/opt/homebrew/lib'
	'/usr/lib'
	'/usr/local/lib'
	)
]
