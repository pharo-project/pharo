extent functions
scaleTo: anExtent
	"Return a Point scalefactor for shrinking a thumbnail of the receiver's extent to fit within anExtent. self and anExtent are expected to have positive nonZero x and y."
	
	| factor  sX sY | 
	factor :=  3.0 reciprocal. "EccentricityThreshhold reciprical"
	sX := anExtent x / self  x asFloat.
	sY :=  anExtent y / self  y asFloat.
	sX = sY ifTrue: [ ^ sX @ sY ]. "Same aspect ratio"
	^ sX < sY 
		ifTrue: [ sX @ (sX max: sY * factor) ] 
		ifFalse: [ (sY max: sX * factor ) @ sY  ] 