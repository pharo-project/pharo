comparing
<= aPoint 
	"Answer whether the receiver is neither below nor to the right of aPoint."

	^ x <= aPoint x and: [y <= aPoint y]