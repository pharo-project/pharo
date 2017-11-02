Extension { #name : #Color }

{ #category : #'*Graphics-Canvas' }
Color >> fillRectangle: aRectangle on: aCanvas [
	"Fill the given rectangle on the given canvas with the receiver."
	
	aCanvas fillRectangle: aRectangle basicFillStyle: self
]
