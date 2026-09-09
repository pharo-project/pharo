Extension { #name : 'AbstractFont' }

{ #category : '*Text-Core' }
AbstractFont >> pixelSize [
	"Make sure that we don't return a Fraction"
	^ TextStyle pointsToPixels: self pointSize
]

{ #category : '*Text-Core' }
AbstractFont >> textStyle [
	^ TextStyle actualTextStyles detect:
		[:aStyle | aStyle fontArray includes: self] ifNone: [ TextStyle fontArray: { self } ]
]
