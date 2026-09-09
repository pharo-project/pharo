"
I find libraries in a unix (64bit) system.
"
Class {
	#name : 'FFIUnix64LibraryFinder',
	#superclass : 'FFIUnixLibraryFinder',
	#category : 'UnifiedFFI-Libraries',
	#package : 'UnifiedFFI',
	#tag : 'Libraries'
}

{ #category : 'accessing' }
FFIUnix64LibraryFinder >> knownPaths [

	^ #(
	"regular paths"
	'/lib/x86_64-linux-gnu'
	'/lib64'
	'/usr/lib64'
	'/usr/lib'
	'/usr/lib/x86_64-linux-gnu'
	'/usr/local/lib'
	"flatpak external paths"
	'/run/host/lib/x86_64-linux-gnu'
	'/run/host/lib64'
	'/run/host/usr/lib64'
	'/run/host/usr/lib'
	'/run/host/usr/lib/x86_64-linux-gnu'
	'/run/host/usr/local/lib'	
)
]
