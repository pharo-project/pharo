Extension { #name : #InfiniteForm }

{ #category : #'*Athens-Morphic' }
InfiniteForm >> asAthensPaintOn: canvas [
	^ (canvas surface createFormPaint: self asForm)
		repeat;
		origin: self origin;
		direction: self direction;
		yourself
]
