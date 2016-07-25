comparing
>= aPoint
	"Answer whether the receiver is neither above nor to the left of aPoint."

	^ x >= aPoint x and: [ y >= aPoint y ]