Extension { #name : #MacOSPlatform }

{ #category : #'*UnifiedFFI' }
MacOSPlatform >> ffiModuleName: aLibrary [ 
	^ aLibrary macModuleName
]
