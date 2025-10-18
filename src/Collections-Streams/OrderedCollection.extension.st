Extension { #name : 'OrderedCollection' }

{ #category : '*Collections-Streams' }
OrderedCollection class >> new: size streamContents: aBlock [
	^ self withAll: (super new: size streamContents: aBlock)
]
