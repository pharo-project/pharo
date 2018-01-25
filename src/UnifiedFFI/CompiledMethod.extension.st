Extension { #name : #CompiledMethod }

{ #category : #'*UnifiedFFI' }
CompiledMethod >> ffiArgumentNames [
	"Answer the method's argument names. We using a separate method, 
	to get arg names not from source code directly, but from method properties, collected at
	compile time. Useful, when there is no source code available (for some reason)"
	^ self isFFIMethod
		ifTrue: [ self properties argsNames ]
		ifFalse: [ self halt. self argumentNames ]
]

{ #category : #'*UnifiedFFI' }
CompiledMethod >> isFFIMethod [
	^ self properties isFFIMethodState
]
