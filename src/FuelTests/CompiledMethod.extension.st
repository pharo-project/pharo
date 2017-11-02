Extension { #name : #CompiledMethod }

{ #category : #'*FuelTests' }
CompiledMethod >> isEqualRegardlessTrailerTo: aCompiledMethod [
	^ (self copyWithTrailerBytes: CompiledMethodTrailer empty) = (aCompiledMethod copyWithTrailerBytes: CompiledMethodTrailer empty)
]

{ #category : #'*FuelTests' }
CompiledMethod >> sizeWithoutTrailer [
	^ self trailer endPC
]
