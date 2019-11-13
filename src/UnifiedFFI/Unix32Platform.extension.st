Extension { #name : #Unix32Platform }

{ #category : #'*UnifiedFFI' }
Unix32Platform >> ffiLibraryName: aLibrary [ 
	^ aLibrary unix32LibraryName
]
