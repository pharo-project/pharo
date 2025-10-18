Extension { #name : 'LinkedList' }

{ #category : '*Collections-Streams' }
LinkedList class >> new: size streamContents: aBlock [
	^ self withAll: (super new: size streamContents: aBlock)
]
