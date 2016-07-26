accessing
fractionRectangle
	"Return a rectangle representing the fraction part of layout frame: i.e left@top corner: right@bottom"
	
	^ leftFraction @ topFraction corner: rightFraction @ bottomFraction 