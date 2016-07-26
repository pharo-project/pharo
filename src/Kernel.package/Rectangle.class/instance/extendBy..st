rectangle functions
extendBy: deltaMargin 
	"Answer a Rectangle with the same origin as the receiver, but whose corner is offset by deltaMargin."

	^ deltaMargin asMargin extendRectangle: self