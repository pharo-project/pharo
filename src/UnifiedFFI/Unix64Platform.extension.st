Extension { #name : #Unix64Platform }

{ #category : #'*UnifiedFFI' }
Unix64Platform >> ffiModuleName: aLibrary [ 
	^ aLibrary unix64ModuleName
]
