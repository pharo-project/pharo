"
I find libraries in a unix (64bit) system.
"
Class {
	#name : #FFIUnix64LibraryFinder,
	#superclass : #FFIUnixLibraryFinder,
	#category : #'UnifiedFFI-Libraries'
}

{ #category : #accessing }
FFIUnix64LibraryFinder >> knownPaths [
		
	^ #(
	'/lib/x86_64-linux-gnu'
	'/lib64'
	'/usr/lib64'	
	'/usr/lib'
	'/usr/local/lib')
]
