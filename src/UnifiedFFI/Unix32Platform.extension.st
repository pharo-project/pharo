Extension { #name : #Unix32Platform }

{ #category : #'*UnifiedFFI' }
Unix32Platform >> ffiModuleName: aLibrary [ 
	^ aLibrary unix32ModuleName
]
