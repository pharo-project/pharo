Extension { #name : 'CompiledMethod' }

{ #category : '*Fuel-Core-Tests' }
CompiledMethod >> isEqualRegardlessTrailerTo: aCompiledMethod [
	^ self copy clearSourcePointer = aCompiledMethod copy clearSourcePointer
]
