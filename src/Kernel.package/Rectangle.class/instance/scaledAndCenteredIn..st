transforming
scaledAndCenteredIn: aRect
	"Answer a new rectangle that fits into aRectangle and is centered
	but with the same aspect ratio as the receiver."
	
	^self width / aRect width > (self height / aRect height)
		ifTrue: [aRect left @ (aRect leftCenter y - (self height * (aRect width / self width) / 2))
					corner: aRect right @ (aRect rightCenter y + (self height * (aRect width / self width) / 2))]
		ifFalse: [aRect topCenter x - (self width * (aRect height / self height) / 2) @ aRect top
					corner: (aRect topCenter x + (self width * (aRect height / self height) / 2)) @ aRect bottom]