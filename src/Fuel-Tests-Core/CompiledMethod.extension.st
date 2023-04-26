Extension { #name : #CompiledMethod }

{ #category : #'*Fuel-Tests-Core' }
CompiledMethod >> isEqualRegardlessTrailerTo: aCompiledMethod [
	^ (self copy setSourcePointer: 0) = (aCompiledMethod copy setSourcePointer: 0)
]
