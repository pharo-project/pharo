Extension { #name : #MacOSPlatform }

{ #category : #'*UnifiedFFI' }
MacOSPlatform >> ffiLibraryName: aLibrary [ 
	^ aLibrary macLibraryName
]
