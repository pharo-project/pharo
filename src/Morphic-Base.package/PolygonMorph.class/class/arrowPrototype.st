instance creation
arrowPrototype
	"Answer an instance of the receiver that will serve as a prototypical arrow"
	"PolygonMorph arrowPrototype openInWorld"
	
	| aa |
	aa := self new. 
	aa vertices: (Array with: 0@0 with: 40@40) 
		color: Color black 
		borderWidth: 2 
		borderColor: Color black.
	aa makeForwardArrow.	
	aa computeBounds.
	^ aa