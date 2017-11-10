Extension { #name : #Collection }

{ #category : #'*GT-Spotter' }
Collection >> gtIterator [
	^ [ :filter | self do: filter ] gtIterator
]
