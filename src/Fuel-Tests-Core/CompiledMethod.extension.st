Extension { #name : #CompiledMethod }

{ #category : #'*Fuel-Tests-Core' }
CompiledMethod >> isEqualRegardlessTrailerTo: aCompiledMethod [
	^ (self copyWithTrailerBytes: CompiledMethodTrailer empty) = (aCompiledMethod copyWithTrailerBytes: CompiledMethodTrailer empty)
]
