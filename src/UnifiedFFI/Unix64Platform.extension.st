Extension { #name : #Unix64Platform }

{ #category : #'*UnifiedFFI' }
Unix64Platform >> ffiLibraryName: aLibrary [ 
	^ aLibrary unix64LibraryName
]
