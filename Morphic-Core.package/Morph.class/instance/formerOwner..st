dropping/grabbing
formerOwner: aMorphOrNil 
	aMorphOrNil ifNil: [self removeProperty: #formerOwner]
		ifNotNil: [self setProperty: #formerOwner toValue: aMorphOrNil]