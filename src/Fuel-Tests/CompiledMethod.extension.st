Extension { #name : #CompiledMethod }

{ #category : #'*Fuel-Tests' }
CompiledMethod >> isEqualRegardlessTrailerTo: aCompiledMethod [
	^ (self copyWithTrailerBytes: CompiledMethodTrailer empty) = (aCompiledMethod copyWithTrailerBytes: CompiledMethodTrailer empty)
]

{ #category : #'*Fuel-Tests' }
CompiledMethod >> sizeWithoutTrailer [
	^ self trailer endPC
]
