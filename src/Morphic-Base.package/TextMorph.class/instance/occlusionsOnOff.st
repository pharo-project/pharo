containment
occlusionsOnOff
	"Establish a container for this text, with opposite occlusion avoidance status"
	self setContainer:
	(container
	ifNil: [(TextContainer new for: self minWidth: textStyle lineGrid*2)
							fillsOwner: false; avoidsOcclusions: true]
	ifNotNil: [(container avoidsOcclusions and: [container fillsOwner not])
			ifTrue: [nil  "Return to simple rectangular bounds"]
			ifFalse: [container avoidsOcclusions: container avoidsOcclusions not]])