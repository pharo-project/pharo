Extension { #name : #InfiniteForm }

{ #category : #'*FormCanvas-Core' }
InfiniteForm >> fillRectangle: aRectangle on: aCanvas [
	"Fill the given rectangle on the given canvas with the receiver."
	
	aCanvas fillRectangle: aRectangle basicFillStyle: self
]
