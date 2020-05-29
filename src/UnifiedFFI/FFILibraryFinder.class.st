"
I'm a tool to find paths in diverse platforms (since they have the tendency of put its libraries in diferent places).
You do not use FFILibraryFinder directly, but one of my children.

Example: 

FFIUnix64LibraryFinder findLibrary: 'libcairo.2.so'
"
Class {
	#name : #FFILibraryFinder,
	#superclass : #Object,
	#category : #'UnifiedFFI-Libraries'
}

{ #category : #accessing }
FFILibraryFinder class >> findLibrary: aName [

	^ self new findLibrary: aName
]

{ #category : #accessing }
FFILibraryFinder >> basePaths [

	^ self subclassResponsibility
]

{ #category : #accessing }
FFILibraryFinder >> findLibrary: aString [
	
	self paths do: [ :each | | path |
		path := each / aString.
		path exists 
			ifTrue: [ ^ path fullName ] ].
	
	self error: ('Cannot locate {1}. Please check if it installed on your system' format: { aString })
]

{ #category : #accessing }
FFILibraryFinder >> knownPaths [
		
	^ self subclassResponsibility
]

{ #category : #accessing }
FFILibraryFinder >> ldLibraryPaths [

	^ (OSEnvironment current at: 'LD_LIBRARY_PATH' ifAbsent: [ '' ]) substrings: ':'
]

{ #category : #accessing }
FFILibraryFinder >> paths [

	^ (Array streamContents: [ :stream | 
		stream 
			nextPutAll: self basePaths;
			nextPutAll: self ldLibraryPaths;
			nextPutAll: self knownPaths ])
		collect: [ :each | each asFileReference ]
]
