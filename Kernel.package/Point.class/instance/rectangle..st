rectangle creation
rectangle: aPoint 
	"Answer a Rectangle that encompasses the receiver and aPoint. This is the most general infix way to create a rectangle."

	^ Rectangle 
		point: self
		point: aPoint