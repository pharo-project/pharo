Extension { #name : #CompiledMethod }

{ #category : #'*Fuel-Tests-Core' }
CompiledMethod >> isEqualRegardlessTrailerTo: aCompiledMethod [
	^ self copy clearSourcePointer = aCompiledMethod copy clearSourcePointer
]
