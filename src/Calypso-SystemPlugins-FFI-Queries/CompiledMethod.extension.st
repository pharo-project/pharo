Extension { #name : #CompiledMethod }

{ #category : #'*Calypso-SystemPlugins-FFI-Queries' }
CompiledMethod >> isFFICall [

	(self numLiterals >= 1 and: [(self literalAt: 1) class == ExternalLibraryFunction])
		ifTrue: [ ^true ].
	
	^self sendsAnySelectorOf: #( 
		ffiCall: ffiCall:module: ffiCall:options: ffiCall:module:options: 
		nbCall: nbCall:module: nbCall:options: nbCall:module:options: 
	)
]
