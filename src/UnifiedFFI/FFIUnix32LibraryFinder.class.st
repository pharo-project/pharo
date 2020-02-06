"
I find libraries in a unix (32bit) system.
"
Class {
	#name : #FFIUnix32LibraryFinder,
	#superclass : #FFIUnixLibraryFinder,
	#category : #'UnifiedFFI-Libraries'
}

{ #category : #accessing }
FFIUnix32LibraryFinder >> knownPaths [
		
	^ #( 
	'/usr/lib/i386-linux-gnu'
	'/usr/lib32'
	'/usr/lib')
]
