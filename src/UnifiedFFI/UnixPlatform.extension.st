Extension { #name : #UnixPlatform }

{ #category : #'*UnifiedFFI' }
UnixPlatform >> ffiModuleName: aLibrary [ 
	^ aLibrary unixModuleName
]
