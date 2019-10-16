Extension { #name : #UnixPlatform }

{ #category : #'*UnifiedFFI' }
UnixPlatform >> ffiLibraryName: aLibrary [ 
	^ aLibrary unixLibraryName
]
