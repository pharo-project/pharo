Extension { #name : 'DisplayMedium' }

{ #category : '*Morphic-Core' }
DisplayMedium >> border: aRectangle width: borderWidth [
	"Paint a border whose rectangular area is defined by aRectangle. The
	width of the border of each side is borderWidth. Uses black for
	drawing the border."

	self border: aRectangle width: borderWidth fillColor: Color black
]
