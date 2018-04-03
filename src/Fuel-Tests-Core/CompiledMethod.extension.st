Extension { #name : #CompiledMethod }

{ #category : #'*Fuel-Tests-Core' }
CompiledMethod >> isEqualRegardlessTrailerTo: aCompiledMethod [
	^ (self copyWithTrailerBytes: CompiledMethodTrailer empty) = (aCompiledMethod copyWithTrailerBytes: CompiledMethodTrailer empty)
]

{ #category : #'*Fuel-Tests-Core' }
CompiledMethod >> sizeWithoutTrailer [
	^ self trailer endPC
]
