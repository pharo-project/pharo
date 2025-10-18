Extension { #name : 'Symbol' }

{ #category : '*Collections-Streams' }
Symbol class >> new: size streamContents: aBlock [
	^ (super new: size streamContents: aBlock) asSymbol
]
